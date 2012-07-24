;;;; class-core.lisp

(in-package #:eshop)

(defmacro class-core.define-class (name slot-list)
  "Macro for making class by given list of slots"
  `(defclass ,name ()
     ,(mapcar #'(lambda (field)
                  `(,(getf field :name)
                     :initarg ,(anything-to-keyword (getf field :name))
                     :initform ,(getf field :initform)
                     :accessor ,(getf field :name)))
              slot-list)))

(defgeneric class-core.make-fields (object)
  (:documentation "Method for viewing slots of item by its obejct-field.*-field-view function"))

(defmacro class-core.define-view-method (name slot-list)
  `(defmethod class-core.make-fields ((object ,name))
     (list
      ,@(mapcar #'(lambda (field)
                    `(slots.%view ',(getf field :type)
                                  (,(getf field :name) object)
                                  ,(format nil "~A" (getf field :name))
                                  ,(getf field :disabled)))
                slot-list))))

(defgeneric class-core.edit-fields (object post-data-plist)
  (:documentation "Method for edit slot values of object according to post-data"))

(defmacro class-core.define-edit-method (name slot-list)
  `(defmethod class-core.edit-fields ((object ,name) post-data-plist)
     (setf
       ,@(mapcan #'(lambda (field)
                     (unless (getf field :disabled)
                       `((,(getf field :name) object)
                         (slots.%get-data ',(getf field :type)
                                          (getf post-data-plist
                                                ,(anything-to-keyword
                                                  (getf field :name)))))))
                 slot-list))))

(defmethod class-core.decode (in-string (dummy group-filter))
  "Decode fullfilter"
  (when (valid-string-p in-string)
    (let ((*package* (find-package :eshop))
          (tmp (read-from-string in-string)))
      (make-instance 'group-filter
                     :name (getf tmp :name)
                     :base (getf tmp :base)
                     :advanced (getf tmp :advanced)))))


(defmacro class-core.define-unserialize-method (name slot-list)
  `(progn
     (defmethod %unserialize (raw (dummy ,name))
       "Make an object with read from file fields"
       (make-instance
        ',name
        ,@(mapcan
           #'(lambda (field)
               (let ((name (anything-to-keyword (getf field :name)))
                     (initform (getf field :initform)))
                 `(,name
                   (let ((val (cdr (assoc ,name raw))))
                     (if val
                         (slots.%decode-from-string ',(getf field :type) val)
                         ,initform)))))
           slot-list)))
     (defmethod %unserialize-from-file (filepath (dummy ,name) storage)
       "Read from file and decode json"
       (with-open-file (file filepath)
         (let ((file-length (cl:file-length file))
               (percent 0))
           (loop
              :for line := (read-line file nil 'EOF)
              :until (eq line 'EOF)
              :do
              (let ((item (%unserialize (decode-json-from-string line)
                                        dummy))
                    (cur-pos (round (* 100 (/ (cl:file-position file) file-length)))))
                (when (> cur-pos percent)
                  (setf percent cur-pos)
                  (when (zerop (mod percent 10))
                    (log5:log-for info-console "Done percent: ~a%" percent)))
                (setf (gethash (key item) storage) item))))))))

(defun class-core.bind-product-to-group (product group)
  "Bind product to group, and push product to group's children"
  (when (every #'(lambda (parent)
                   (string/= (key group) (key parent)))
               (parents product))
    (pushnew group (parents product)))
  (when (every #'(lambda (child)
                   (string/= (key product) (key child)))
               (products group))
    (pushnew product (products group))))


(defgeneric %post-unserialize (item)
  (:documentation "Method that called after unserializing all the items from files.
Usually it transform string keys to pointers to other objects, such as parents or childs."))

(defgeneric %post-unserialize-item (item)
  (:documentation "Processing individual item after unserialize"))

;;; TODO: make this method standard using "before" keyword
(defmethod %post-unserialize (dummy)
  "Do standard post-unserialize actions with each item of same type as dummy
Reload this method if more actions required"
  (maphash #'(lambda (k v)
               (declare (ignore k))
               (%post-unserialize-item v))
           (get-storage (type-of dummy))))

;;вызывается после десереализации продукта
(defmethod %post-unserialize-item ((item product))
  ;; после десериализации в parent лежит список key родительских групп
  ;; TODO: make checks for existance
  (setf (parents item)
        (keys-to-objects (parents item) :type 'group))
  ;; проставление ссылок у родителей на данный продукт
  (mapcar #'(lambda (parent)
              (push item (products parent)))
          (parents item))
  ;; проверка цены, если цена в ИМ ноль, а дельта положительная нужно изменить цену
  ;; TODO: вынести эту проверку в отдельный метод (использовать не только на старте)
  (when (and (zerop (siteprice item))
             (plusp (delta-price item)))
    (setf (siteprice item) (delta-price item))
    (setf (delta-price item) 0))
  ;;active - если имеется в наличии и цена > 0
  (setf (active item) (and (plusp (count-total item)) (plusp (siteprice item))))
  ;; setting product vendor
  (setf (vendor item) (get-option item "Общие характеристики" "Производитель")))


(defmethod %post-unserialize-item ((item group))
  ;; upsale
  (setf (upsale-links item)
        (mapcar #'(lambda (group-key)
                    (when group-key
                      (getobj group-key 'group)))
                (upsale-links item)))
  ;; после десериализации в parent лежит список key родительских групп
  (setf (parents item)
        (keys-to-objects (parents item) :type 'group))
  ;; проставление ссылок у родителей на данную группу
  (mapcar #'(lambda (parent)
              (push item (groups parent)))
          (parents item))
  (setf (empty item) (notany #'active (products item)))
  (when (and (raw-fullfilter item)
             (null (fullfilter item)))
    (setf (fullfilter item) (class-core.decode (raw-fullfilter item) (make-instance 'group-filter)))))

(defmethod %post-unserialize-item ((item filter))
  ;; eval func-string to func
  (when (valid-string-p (func-string item))
    (setf (func item) (eval (read-from-string (func-string item)))))
  ;; после десериализации в parent лежит список key родительских групп
  (setf (parents item)
        (keys-to-objects (parents item) :type 'group))
  ;; проставление ссылок у родителей на данную группу
  (mapcar #'(lambda (parent)
              (push item (filters parent)))
          (parents item)))


(defmethod %post-unserialize-item ((item vendor))
  "Do post-unserialize actions with vendor object"
  ;; convert seo-texts from list to hash-table
  (when (listp (seo-texts item))
    (setf (seo-texts item) (servo.list-to-hashtasble
                            (copy-list (seo-texts item)))))
  ;; make pointers to vendor in group's hashtable of vendors
  (let ((vendor-key (key item)))
    (maphash #'(lambda (k v)
                 (declare (ignore v))
                 (let ((group (getobj k 'group)))
                   (setf (gethash vendor-key (vendors group)) item)))
             (seo-texts item))))

(defmethod %post-unserialize ((dummy vendor))
  "Do post-unserialize actions with vendor storage.
Reloaded standard method %post-unserialize"
  (let ((storage (get-storage 'vendor)))
    (maphash #'(lambda (k v)
                 (declare (ignore k))
                 (%post-unserialize-item v))
             storage)
    (maphash #'(lambda (k v)
                 (declare (ignore k))
                 (let ((name (name v)))
                   (when (valid-string-p name)
                     (setobj (string-downcase name) v 'vendor))))
             (copy-structure storage))))

(defun class-core.unserialize-all ()
  "Unsrialize all classes objects from files"
  ;; unserialize all instances, without processing slots
  (maphash
   #'(lambda (class properties)
       (let ((t-storage (make-hash-table :test #'equal)) ; storage for temp instances
             (instance (get-instance class)))
         (log5:log-for info "Unserialize ~A..." class)
         (%unserialize-from-file (get-last-bakup-pathname class)
                                 instance
                                 t-storage)
         ;; set real storage as fully unserialized temp storage
         (setf (getf properties :storage) t-storage)))
   *classes*)
  ;; post-unserialize actions, such as making links instead of text keys
  ;; Note: can't be merged with previous maphash, because must be after it
  (maphash
   #'(lambda (class properties)
       (declare (ignore properties))
       (%post-unserialize (get-instance class)))
   *classes*))

(defparameter *classes* (make-hash-table :test #'equal)
  "Hash-table of all classes, containing plist of options,
such as pointer to storage, serialize flag, etc.")


(defgeneric get-instance (type)
  (:documentation "Get singleton instance of given type (usually used as dummy for methods)"))

(defmethod get-instance ((type symbol))
  (getf (gethash type *classes*) :instance))

(defmethod get-instance ((type string))
  (get-instance (anything-to-symbol type)))

(defun get-last-bakup-pathname (type)
  "Return pathname for file of last backup objects of given type"
  (declare (symbol type))
  (if (not (gethash type *classes*))
      (error "type ~A doesn't exist" type)
      ;;else
      (merge-pathnames (format nil "~(~A~).bkp" type)
                       (config.get-option "PATHS" "path-to-last-backup"))))

(defmacro class-core.define-class-checker (name)
  "Macro for defining type-checking functions such as productp, groupp, etc"
  `(defun ,(intern (format nil "~:@(~Ap~)" name)) (object)
     ,(format nil "Return T if OBJECT is a ~A, and NIL otherwise." name)
     (typep object ',name)))

(defmacro class-core.make-class-and-methods (name slot-list &key (serialize t)
                                             (make-storage t) storage-size
                                             instance-initforms)
  "Make class, storage for its instances (if needed) and necessary methods for it
\(such as serialization, unserialization, viewing, editing, etc)"
  `(values
     ;; make plist of properties of class
     ;; new properties will be added later it this macro
     (setf (gethash ',name *classes*) (list :serialize ,serialize))
     (class-core.define-class ,name ,slot-list)
     (class-core.define-class-checker ,name)
     ;; make singleton instance of class
     (setf (getf (gethash ',name *classes*) :instance)
           (make-instance ',name ,@instance-initforms))
     (class-core.define-view-method ,name ,slot-list)
     (class-core.define-edit-method ,name ,slot-list)
     ,@(when make-storage
             `(;; set :storage property if class as pointer to real storage
               (setf (getf (gethash ',name *classes*) :storage)
                     (make-hash-table :test #'equal ,@(awhen storage-size
                                                             (list :size it))))
               (class-core.define-unserialize-method ,name ,slot-list)
               ,@(when serialize
                       `((backup.define-serialize-method ,name ,slot-list)))))))

(defun keys-to-objects (key-list &key type (remove-if #'null) default key)
  "Returns list of objects corresponding to given list of keys.
If parameter type is given, use this type, otherwise doesnt check types.
Remove elements from result list corresponding to remove-func"
  ;;; TODO: make as method not only on lists
  (declare (symbol type))
  (let ((key (sequence:canonize-key key)))
    (remove-if remove-if
               (mapcar #'(lambda (key-obj)
                           (getobj (funcall key key-obj) type default))
                       key-list))))

(defun parent (item)
  "Returns main parent of item"
  (car (parents item)))

(defun class-core.get-root-parent (item)
  (when item
    (let ((parent (parent item)))
      (if (null parent)
          item
          (class-core.get-root-parent parent)))))

(defun class-exist-p (class)
  "Checks whether system has class with given name"
  (declare (symbol class))
  (not (null (gethash class *classes*))))

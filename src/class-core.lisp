;;;; class-core.lisp

(in-package #:eshop)

(defgeneric class-core.post-unserialize (item)
  (:documentation "Method that called after unserializing all the items from files.
Usually it transform string keys to pointers to other objects, such as parents or childs."))

(defmacro class-core.define-class (name slot-list)
  "Macro for making class by given list of slots"
  `(defclass ,name ()
     ,(mapcar #'(lambda (field)
                  `(,(getf field :name)
                     :initarg ,(intern (format nil "~A" (getf field :name)) :keyword)
                     :initform ,(getf field :initform)
                     :accessor ,(getf field :name)))
              slot-list)))


(defmacro class-core.define-view-method (name slot-list)
  `(defmethod class-core.make-fields ((object ,name))
     (list
      ,@(mapcar #'(lambda (field)
                    `(,(intern
                        (format nil "~:@(object-fields.~A-field-view~)"
                                (getf field :type)))
                       (,(getf field :name)  object)
                       ,(format nil "~a" (getf field :name))
                       ,(getf field :disabled)))
                slot-list))))

(defmacro class-core.define-edit-method (name slot-list)
  `(defmethod class-core.edit-fields ((object ,name) post-data-plist)
     (setf
       ,@(mapcan #'(lambda (field)
                     (unless (getf field :disabled)
                       `((,(getf field :name) object)
                         (,(intern
                            (format nil "~:@(object-fields.~A-field-get-data~)"
                                    (getf field :type)))
                           (getf post-data-plist
                                 ,(intern (format nil "~:@(~A~)"
                                                 (getf field :name))
                                         :keyword))))))
                 slot-list))))

(defmethod class-core.decode (in-string (dummy group-filter))
  "Decode fullfilter"
  (when (servo.valid-string-p in-string)
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
               (let ((name (intern (format nil "~:@(~A~)" (getf field :name)) :keyword))
                     (initform (getf field :initform)))
                 `(,name
                   (let ((val (cdr (assoc ,name raw))))
                     (if val
                         val
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
              (let ((item (unserialize (decode-json-from-string line)
                                       dummy))
                    (cur-pos (round (* 100 (/ (cl:file-position file) file-length)))))
                (when (> cur-pos percent)
                  (setf percent cur-pos)
                  (when (zerop (mod percent 10))
                    (log5:log-for info-console "Done percent: ~a%" percent)))
                (setf (gethash (key item) storage) item))))))))

(defun class-core.get-transform-optgroups (item)
  (declare (product item))
  ;;преобразуем значение :options в plist (2 уровень)
  (mapcar #'(lambda (optgroup)
              (let ((optgroup-plist
                     (mapcar #'servo.alist-to-plist (getf optgroup :options))))
                (list :name (getf optgroup :name) :options optgroup-plist)))
          ;; преобразуем optgroups (1 уровень)
          (mapcar #'servo.alist-to-plist (optgroups item))))


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
  (setf (parents item)
        (mapcar #'(lambda (parent-key)
                    (when parent-key
                      (let ((parent (getobj parent-key 'group)))
                        ;; if parent group exists, bind that group with product
                        (when parent
                          (push item (products parent))
                          parent))))
                (parents item)))
  ;; проверка цены, если цена в ИМ ноль, а дельта положительная нужно изменить цену
  ;; TODO: вынести эту проверку в отдельный метод (использовать не только на старте)
  (when (and (zerop (siteprice item))
             (plusp (delta-price item)))
    (setf (siteprice item) (delta-price item))
    (setf (delta-price item) 0))
  ;;active - если имеется в наличии и цена > 0
  (setf (active item) (and (plusp (count-total item)) (plusp (siteprice item))))
  ;;adding newlines instead of #Newline
  (setf (seo-text item) (object-fields.string-add-newlines (seo-text item)))
  ;;преобразуем optgroups из списка alist в список plist
  (setf (optgroups item) (class-core.get-transform-optgroups item))
  ;; setting product vendor
  (with-option1 item "Общие характеристики" "Производитель"
                (setf (vendor item) (getf option :value))))


(defmethod %post-unserialize-item ((item group))
  ;;adding newlines instead of #Newline
  (when (seo-text item)
    (setf (seo-text item) (object-fields.string-add-newlines (seo-text item))))
  ;; upsale
  (setf (upsale-links item)
        (mapcar #'(lambda (group-key)
                    (when group-key
                      (getobj group-key 'group)))
                (upsale-links item)))
  ;; после десериализации в parent лежит список key родительских групп
  (setf (parents item)
        (remove-if #'null (parents item)
                   :key (make-curry-lambda getobj 'group)))
  ;; проставление ссылок у родителей на данную группу
  (mapcar #'(lambda (parent)
              (push item (groups parent)))
          (parents item))
  (setf (empty item) (some #'active (products item)))
  (setf (keyoptions item) (mapcar #'(lambda (pair)
                                      (list :optgroup (cdr (assoc :optgroup pair))
                                            :optname (cdr (assoc :optname pair))))
                                  (keyoptions item)))
  ;;catalog-keyoptions
  (setf (catalog-keyoptions item)
        (mapcar #'(lambda (item)
                    (list :optgroup (cdr (assoc :optgroup item))
                          :optname (cdr (assoc :optname item))
                          :showname (cdr (assoc :showname item))
                          :units (cdr (assoc :units item))))
                (catalog-keyoptions item)))
  (when (and (raw-fullfilter item)
             (null (fullfilter item)))
    (setf (raw-fullfilter item) (object-fields.string-add-newlines (raw-fullfilter item)))
    (setf (fullfilter item) (class-core.decode (raw-fullfilter item) (make-instance 'group-filter)))))

(defmethod %post-unserialize-item ((item filter))
  ;; add newlines instead of #Newline
  (setf (func-string item) (object-fields.string-add-newlines (func-string item)))
  ;; eval func-string to func
  (setf (func item) (eval (read-from-string (func-string item))))
  ;; после десериализации в parent лежит список key родительских групп
  (setf (parents item)
        (remove-if #'null
                   (mapcar #'(lambda (parent-key)
                               (when parent-key
                                 (let ((parent (getobj parent-key 'group)))
                                   ;; if parent (group) exists, bind him with filter
                                   (when parent
                                     (push item (filters parent))
                                     parent))))
                           (parents item)))))


(defmethod %post-unserialize-item ((item vendor))
  "Do post-unserialize actions with vendor object"
  ;; convert seo-texts from list to hash-table
  (when (listp (seo-texts item))
    (setf (seo-texts item) (servo.list-to-hashtasble
                            (copy-list (seo-texts item)))))
  (loop :for k :being :the hash-keys :in (seo-texts item)
     :do (setf (gethash k (seo-texts item))
               (object-fields.string-add-newlines (gethash k (seo-texts item)))))
  ;; make pointers to vendor in group's hashtable of vendors
  (let ((vendor-key (key item)))
    (maphash #'(lambda (k v)
                 (declare (ignore v))
                 (let ((group (getobj k 'group)))
                   (setf (gethash vendor-key (vendors group)) item)))
             (seo-texts item))))

(defmethod %post-unserialize ((dummy vendor))
  "Do post-unserialize actions with vendor storage.
Reloaded sandard method %post-unserialize"
  (let ((storage (get-storage 'vendor)))
    (maphash #'(lambda (k v)
                 (declare (ignore k))
                 (%post-unserialize-item v))
             storage)
    (maphash #'(lambda (k v)
                 (declare (ignore k))
                 (let ((name (name v)))
                   (when (servo.valid-string-p name)
                     (setobj 'vendor (string-downcase name) v))))
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
         (setf (getf properties :stroage) t-storage)))
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
  (get-instance (intern (format nil "~:@(~A~)" type))))

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
  `(defun ,(intern (format nil "~:@(~Ap~)" name)) (obj)
     ,(format nil "Checks if the object is of type ~A" name)
     (typep obj ,name)))

(defmacro class-core.make-class-and-methods (name slot-list &key (serialize t)
                                             (make-storage t) storage-name storage-size)
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
           (make-instance ',name))
     (class-core.define-view-method ,name ,slot-list)
     (class-core.define-edit-method ,name ,slot-list)
     ,@(when make-storage
            (let ((storage-name (aif storage-name
                                     it
                                     (intern (format nil "*~@:(~A-storage~)*" name)))))
              `((defparameter ,storage-name
                  (make-hash-table :test #'equal ,@(awhen storage-size
                                                          '(:size it)))
                  ,(format nil "Automatically created storage for objects of type ~A" name))
                ;; set :storage property if class as pointer to real storage
                (setf (getf (gethash ',name *classes*) :storage) ,storage-name)
                ,@(when serialize
                        `((class-core.define-unserialize-method ,name ,slot-list)
                          (backup.define-serialize-method ,name ,slot-list))))))))

(defun keys-to-objects (key-list &key type (remove-func #'null) default key)
  "Returns list of objects corresponding to given list of keys.
If parameter type is given, use this type, otherwise doesnt check types.
Remove elements from result list corresponding to remove-func"
  ;;; TODO: make as method not only on lists
  (let ((key (sequence:canonize-key key)))
    (declare (symbol type))
    (remove-if remove-func
               (mapcar #'(lambda (key-obj)
                           (getobj (funcall key key-obj) type default))
                       key-list))))

;; для того чтобы работали фильтры
(defmethod price ((object product))
  (+ (siteprice object) (delta-price object)))

(defun class-core.parent (item)
  "Returns main parent of item"
  (car (parents item)))

(defun class-core.breadcrumbs (in &optional out)
  "Processing parents until nil, creating breadcrumbs"
  (if in
      (progn
        (if (typep in 'product)
            (push (list :key (articul in) :val (name-seo in)) out)
            (push (list :key (key in) :val (name in)) out))
        (class-core.breadcrumbs (class-core.parent in) out))
      ;; else -  end of recursion
      (list :breadcrumbelts (butlast out)
            :breadcrumbtail (car (last out)))))

(defun class-core.get-root-parent (item)
  (when item
    (let ((parent (class-core.parent item)))
      (if (null parent)
          item
          (class-core.get-root-parent parent)))))


(defun menu-sort (a b)
  "Function for sorting groups by order field"
  (when (and (order a) (order b))
    (< (order a)
       (order b))))


;;TODO временно убрана проверка на пустые группы, тк это поле невалидно
(defun class-core.menu (&optional current-object)
  "Creating left menu"
  (let* ((current-root (class-core.get-root-parent current-object))
         (divider-list (list "setevoe-oborudovanie" "foto-and-video" "rashodnye-materialy"))
         (src-lst
          (mapcar #'(lambda (val)
                      (if (and current-root
                               (equal (key val) (key current-root)))
                          ;; This is current
                          (soy.menu:selected
                           (list :divider (notevery #'(lambda (divider)
                                                        (string/= (key val) divider))
                                                    divider-list)
                                 :key (key val)
                                 :name (name val)
                                 :icon (icon val)
                                 :subs (loop
                                          :for child
                                          :in (sort
                                               (remove-if #'(lambda (g)
                                                              (or
                                                               ;; (empty g)
                                                               (not (active g))))
                                                          (groups val))
                                               #'class-core.menu-sort)
                                          :collect
                                          (list :key  (key child) :name (name child)))))
                          ;; else - this is ordinal
                          (soy.menu:ordinal (list :divider (notevery #'(lambda (divider)
                                                                         (string/= (key val) divider))
                                                                     divider-list)
                                                  :key  (key val)
                                                  :name (name val)
                                                  :icon (icon val)))))
                  (get-root-groups))))
    (soy.menu:main (list :elts src-lst))))

(defun class-core.has-vendor-seo-text (group vendor-key)
  "Chech whether there is vendor's seo-text for given group"
  (and group (servo.valid-string-p vendor-key)
       (let ((vendor-obj (gethash (string-downcase vendor-key) (vendors group))))
         (and vendor-obj (gethash (key group) (seo-texts vendor-obj))))
       t))

(defun class-core.get-group-seo-text (group &optional vendor-key)
  "If vendor passed, try to return corresponding seo-text for group,
if there is not one, or no vendor passed, return group's seo-text"
  (declare (group group))
  (let ((vendor-object (when (servo.valid-string-p vendor-key)
                         (gethash (string-downcase vendor-key) (vendors group)))))
    (aif (and vendor-object (gethash (key group) (seo-texts vendor-object)))
         it             ; if condition non-nil, it is required seo-text
         ;; else
         (seo-text group))))

;;;; storage.lisp

(in-package #:eshop)

(defun get-storage (type)
  "Get storage for given type objects"
  (declare (symbol type))
  (getf (gethash type *classes*) :storage))

(defun getobj (key &optional type default)
  "Get object of given type from appropriate storage.
If no type given, search in all storages.
Note: returned object is NOT setfable (but its fields are)"
  (declare (symbol type))
  (if type
      (gethash key
               (get-storage type)
               default)
      ;; else, search in all storages
      (getobj-global key)))

(defun getobj-global (key)
  "Get object regardless of type, from some storage (try to find in all)
Note: returned object is NOT setfable (but its fields are)"
  (let (res)
    (maphash #'(lambda (k v)
                 (declare (ignore v))
                 (awhen (and (not res)  ; find only first (but almost always
                                        ; there should be only one required object)
                             (gethash key (get-storage k)))
                   (setf res it)))
             *classes*)
    res))

(defun setobj (key value)
  "Set/edit object of given type (type of value) in appropriate storage"
  (let ((storage (get-storage (type-of value))))
    (setf (gethash key storage) value)))

(defun remobj (key &optional type)
  "Get object of given type from appropriate storage
If no type given, search in all storages"
  (declare (symbol type))
  (if type
      (remhash key (get-storage type))
      (remobj-global key)))

(defun remobj-global (key)
  (maphash #'(lambda (k v)
               (declare (ignore v))
               (remhash key (get-storage k)))
           *classes*))

(defun process-storage (func type)
  "Process storage of given type apllying given func to each element.
Func should take 1 argument - elt for processing
Note: processed element can't be changed during processing"
  (declare (function func) (symbol type))
  (maphash #'(lambda (k v)
               (declare (ignore k))
               (funcall func v))
           (get-storage type)))

(defun process-storage-with-keys (func type)
  "Same as process-storage, but func should take 2 arguments - key and value"
  (declare (function func) (symbol type))
  (maphash func (get-storage type)))

(defun process-and-collect-storage (type &key (func #'identity) (when-func (constantly t)))
  "Process storage of given type checking via when-func, applying func to
each element and collecting its results"
  (loop
     :for elt :being :the hash-values :in (get-storage type)
     :when (funcall when-func elt)
     :collect (funcall func elt)))

(defun get-root-groups ()
  "Return list of root groups sorted by order"
  (stable-sort
   (process-and-collect-storage 'group :when-func (complement #'parents))
   #'menu-sort))


;;;;; old storage methods below

(defun storage.alphabet-group-sorter (a b)
  (when (and (name a) (name b))
    (string< (name a) (name b))))

(defun storage.get-all-child-groups (root &optional (sorter #'storage.alphabet-group-sorter))
  (sort
   (let ((children (groups root)))
     (if (null children)
         (list root)
         (mapcan (make-curry-lambda storage.get-all-child-groups sorter) children)))
   sorter))

(defmethod storage.get-recursive-products ((object group))
  (append
   (products object)
   (mapcan #'storage.get-recursive-products (groups object))))

(defmethod storage.get-filtered-products ((object group) &optional (filter #'active))
  (remove-if-not filter
                 (storage.get-recursive-products object)))

(defmethod storage.get-filtered-products ((object list) &optional (filter #'active))
  (remove-if-not filter object))

(defgeneric storage.get-vendors (object)
  (:documentation ""))

(defmethod storage.get-vendors ((object group))
  (storage.get-vendors (storage.get-filtered-products object)))

(defmethod storage.get-vendors ((object list))
  (let ((products-list object)
        (vendors (make-hash-table :test #'equal)))
    (mapcar #'(lambda (product)
                (let ((vendor (vendor product)))
                  (when (and vendor (string/= "" vendor))
                    (if (gethash vendor vendors)
                        (incf (gethash vendor vendors))
                        (setf (gethash vendor vendors) 1)))))
            products-list)
    vendors))



(defun storage.round-collect-storage (checker &optional (storage (storage *global-storage*)) (compare t compare-supplied-p))
  "Processing storage (storage should be hash-table) and creating list according to checker function. Sorting with passed comparator"
  (declare (hash-table storage))
  (let ((result
         (loop
            :for elt :being :the hash-value :in storage
            :when (funcall checker elt)
            :collect elt)))
    (if compare-supplied-p
        (stable-sort (copy-list result) compare)
        result)))

(defun storage.get-products-list ()
  (storage.round-collect-storage #'(lambda (obj) (typep obj 'product))))

(defun storage.get-active-products-list ()
  (storage.round-collect-storage #'(lambda (obj) (and (typep obj 'product) (active obj)))))

(defun storage.get-groups-list ()
  (storage.round-collect-storage #'(lambda (obj) (typep obj 'group))))

(defun storage.get-filters-list ()
  (storage.round-collect-storage #'(lambda (obj) (typep obj 'filter))))

(defun storage.get-actual-groups-list ()
  (storage.round-collect-storage #'(lambda (obj) (and (typep obj 'group)
                                                      (not (empty obj))
                                                      (active obj)))))


(defun storage.add-new-object (object storage &optional (key nil key-supplied-p))
  "Adding exactly new object to appropriate storage but not pushing it in any list"
  ;;; TODO: push all types (not only vendors to own storage)
  (let ((key (if key-supplied-p
                 key
                 (key object))))
    (setf (gethash key storage) object)))

(defun storage.edit-in-list (list object &optional (key nil key-supplied-p))
  "Editing or adding (if not exist) object in given list"
  (unless key-supplied-p
    (setf key (key object)))
  (aif (find key list :key #'key)
       (progn
         (setf (nth it list) object)
         list)
       (push object list)))


(defun storage.edit-object (object &optional (key nil key-supplied-p))
  "Editing or adding object to storage and edit it in appropriate lists"
  (unless key-supplied-p
    (setf key (key object)))
  (setf (gethash key (storage *global-storage*)) object)
  (when (typep object 'product)
    (setf (products *global-storage*) (storage.edit-in-list (products *global-storage*) object key))
    (when (active object)
      (setf (active-products *global-storage*) (storage.edit-in-list (active-products *global-storage*) object key))))
  (when (typep object 'group)
    (setf (groups *global-storage*) (storage.edit-in-list (groups *global-storage*) object key))
    (when (and (active object) (not (empty object)))
      (setf (actual-groups *global-storage*) (storage.edit-in-list (actual-groups *global-storage*) object key))))
  (when (typep object 'filter)
    (setf (filters *global-storage*) (storage.edit-in-list (filters *global-storage*) object key))))


(defun storage.make-lists ()
  (setf (groups *global-storage*) (storage.get-groups-list))
  (setf (actual-groups *global-storage*) (storage.get-actual-groups-list))
  (setf (products *global-storage*) (storage.get-products-list))
  (setf (active-products *global-storage*) (storage.get-active-products-list))
  (setf (filters *global-storage*) (storage.get-filters-list)))


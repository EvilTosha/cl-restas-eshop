;;;; storage.lisp

(in-package #:eshop)

(defclass global-storage ()
  ((storage :initarg :storage :initform (make-hash-table :test #'equal) :accessor storage)
   (products :initarg :products :initform nil :accessor products)
   (groups :initarg :groups :initform nil :accessor groups)
   (filters :initarg :filters :initform nil :accessor filters)
   (actual-groups :initarg :actual-groups :initform nil :accessor actual-groups)
   (active-products :initarg :active-products :initform nil :accessor active-products)
   (root-groups :initarg :root-groups :initform nil :accessor root-groups)))


(defvar *global-storage* (make-instance 'global-storage))

(defvar *vendor-storage* (make-hash-table :test #'equal)
  "Storage for vendors (with aliases and seo-texts)")

(defun storage.alphabet-group-sorter (a b)
  (when (and (name a) (name b))
    (STRING< (name a) (name b))))

(defun storage.get-all-child-groups (root &optional (sorter #'storage.alphabet-group-sorter))
  (sort
   (let ((children (groups root))
         (res))
     (if (null children)
         (list root)
         (progn
           (mapcar #'(lambda (root)
                       (setf res (append res (storage.get-all-child-groups root sorter))))
                   children)
           res))) sorter))

(defmethod storage.get-recursive-products ((object group))
  (let ((products (products object)))
    (loop :for child :in (groups object) :do
       (setf products (append products (storage.get-recursive-products child))))
    products))

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

(defun storage.get-vendors-list ()
  (storage.round-collect-storage #'(lambda (obj) (typep obj 'vendor)) *vendor-storage*))

(defun storage.get-filters-list ()
  (storage.round-collect-storage #'(lambda (obj) (typep obj 'filter))))

(defun storage.get-actual-groups-list ()
  (storage.round-collect-storage #'(lambda (obj) (and (typep obj 'group)
                                                      (not (empty obj))
                                                      (active obj)))))


(defun storage.get-root-groups-list (&optional (compare #'(lambda (a b)
                                                            (when (and (order a) (order b))
                                                              (< (order a) (order b))))))
  (storage.round-collect-storage #'(lambda (obj)
                                     (and (typep obj 'group)
                                          (null (parents obj))))
                                 (storage *global-storage*) compare))


(defun storage.add-new-object (object &optional (key nil key-supplied-p))
  "Adding exactly new object to appropriate storage but not pushing it in any list"
  ;;; TODO: push all types (not only vendors to own storage)
  (let ((key (if key-supplied-p
                 key
                 (key object))))
    (setf (gethash key
                   (typecase object
                     (vendor
                      *vendor-storage*)
                     (t
                      (storage *global-storage*))))
          object)))

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
      (setf (actual-groups *global-storage*) (storage.edit-in-list (actual-groups *global-storage*) object key)))
    (unless (new-classes.parent object)
      (setf (root-groups *global-storage*) (storage.edit-in-list (root-groups *global-storage*) object key))))
  (when (typep object 'filter)
    (setf (filters *global-storage*) (storage.edit-in-list (filters *global-storage*) object key))))


(defun storage.make-lists ()
  (setf (groups *global-storage*) (storage.get-groups-list))
  (setf (actual-groups *global-storage*) (storage.get-actual-groups-list))
  (setf (root-groups *global-storage*) (storage.get-root-groups-list))
  (setf (products *global-storage*) (storage.get-products-list))
  (setf (active-products *global-storage*) (storage.get-active-products-list))
  (setf (filters *global-storage*) (storage.get-filters-list)))


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

;;!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
(defmethod storage.main-parent ((object group))
  (car (parents object)))

(defmethod storage.main-parent ((object product))
  (car (parents object)))
;;!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

(defun storage.is-offspring (group item)
  (when (and group item)
    (not (every
          #'null
          (mapcar #'(lambda (parent)
                      (if (equal group parent)
                          t
                          (if (not (null parent))
                              (storage.is-offspring group parent)
                              nil)))
                  (parents item))))))

(defun storage.get-all-child-groups (root &optional (sort-f #'catalog.alphabet-group-sort-f))
  (sort
   (let ((children (groups root))
         (res))
     (if (null children)
         (list root)
         (progn
           (mapcar #'(lambda (root)
                       (setf res (append res (storage.get-all-child-groups root sort-f))))
                   children)
           res))) sort-f))

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



(defun storage.round-collect-storage (checker &optional (compare t compare-supplied-p))
  "Processing storage and creating list according to checker function. Sorting with passed comparator"
  (let ((result))
    (maphash #'(lambda (key node)
                 (declare (ignore key))
                 (when (funcall checker node)
                   (push node result)))
             (storage *global-storage*))
    (if compare-supplied-p
        (stable-sort (copy-list result) compare)
        result)))


(defun storage.get-products-list ()
  (storage.round-collect-storage #'(lambda (obj) (equal (type-of obj) 'product))))

(defun storage.get-active-products-list ()
  (storage.round-collect-storage #'(lambda (obj) (and (equal (type-of obj) 'product) (active obj)))))

(defun storage.get-groups-list ()
  (storage.round-collect-storage #'(lambda (obj) (equal (type-of obj) 'group))))

(defun storage.get-filters-list ()
  (storage.round-collect-storage #'(lambda (obj) (equal (type-of obj) 'filter))))

(defun storage.get-actual-groups-list ()
  (storage.round-collect-storage #'(lambda (obj) (and (equal (type-of obj) 'group) (not (empty obj)) (active obj)))))



(defun storage.get-root-groups-list (&optional (compare #'(lambda (a b)
                                                            (if (or (null (order a)) (null (order b)))
                                                                nil
                                                                (< (order a) (order b))))))
  (storage.round-collect-storage #'(lambda (obj)
                                     (and (equal (type-of obj) 'group)
                                          (null (parents obj))))
                                 compare))




(defun storage.add-new-object (object &optional (key nil key-supplied-p))
  "Adding exactly new object to storage but not pushing it in any list"
  (when (not key-supplied-p)
    (setf key (key object)))
  (setf (gethash key (storage *global-storage*)) object))
  ;; (log5:log-for test "Added new element with key ~a" key))

(defun storage.edit-in-list (list object &optional (key nil key-supplied-p))
  "Editing or adding (if not exist) object in given list"
  (when (not key-supplied-p)
    (setf key (key object)))
  (let* ((found nil)
         (result-list (mapcar #'(lambda (list-obj)
                                  (if (equal (key list-obj) key)
                                      (progn
                                        (setf found t)
                                        object)
                                      list-obj))
                              list)))
    (if (null found)
        (push object list)
        result-list)))


(defun storage.edit-object (object &optional (key nil key-supplied-p))
  "Editing or adding object to storage and edit it in appropriate lists"
  (when (not key-supplied-p)
    (setf key (key object)))
  ;; (log5::log-for test "add/edit ~a" key)
  (setf (gethash key (storage *global-storage*)) object)
  (when (equal (type-of object) 'product)
    (setf (products *global-storage*) (storage.edit-in-list (products *global-storage*) object key))
    (when (active object)
      (setf (active-products *global-storage*) (storage.edit-in-list (active-products *global-storage*) object key))))
  (when (equal (type-of object) 'group)
    (setf (groups *global-storage*) (storage.edit-in-list (groups *global-storage*) object key))
    (when (and (active object) (not (empty object)))
      (setf (actual-groups *global-storage*) (storage.edit-in-list (actual-groups *global-storage*) object key)))
    (when (null (new-classes.parent object))
      (setf (root-groups *global-storage*) (storage.edit-in-list (root-groups *global-storage*) object key))))
  (when (equal (type-of object) 'filter)
    (setf (filters *global-storage*) (storage.edit-in-list (filters *global-storage*) object key))))


(defun storage.make-lists ()
  (setf (groups *global-storage*) (storage.get-groups-list))
  (setf (actual-groups *global-storage*) (storage.get-actual-groups-list))
  (setf (root-groups *global-storage*) (storage.get-root-groups-list))
  (setf (products *global-storage*) (storage.get-products-list))
  (setf (active-products *global-storage*) (storage.get-active-products-list))
  (setf (filters *global-storage*) (storage.get-filters-list)))

(defun storage.group-unbinding (group)
  (mapcar #'(lambda (product)
              (setf (parents product) nil))
          (products group))
  (setf (products group) nil))


;;;; Author: wolforus@gmail.com (Fedor Voronin)

;;;; Black-list items.
(in-package #:eshop)

(defvar black-list.*storage* (make-hash-table :test #'equal)
  "Key - object key, val - instance of black-list.item")

(defstruct (black-list.item
  (:constructor
   black-list.%create-item (item &optional
                                    (add-ts (get-universal-time))
                                    (die-ts (+ add-ts (* 60 60 24 100))))))
  (item nil :type product) ;; bad product itself
  (add-ts (get-universal-time) :type integer) ;; time stamp when it been added
  (die-ts (get-universal-time) :type integer)) ;; time stamp when

(defgeneric black-list.insert (item &optional timestamp)
    (:documentation "Make new instance & insert item to black list"))

(defgeneric black-list.%deactive (item)
    (:documentation "Deactive item from black list"))

;;--- TODO(wolforus@gmail.com): Move to core report see report.lisp
(defgeneric black-list.%print-item (item)
    (:documentation "Print item data for repors"))

;;; Inserting items

(defmethod black-list.insert ((key number) &optional (timestamp (get-universal-time)))
  "Get object by key and add it to black list. Ignore keys for that there are not objects in the storage."
  (black-list.insert (write-to-string key) timestamp))

(defmethod black-list.insert ((key string) &optional (timestamp (get-universal-time)))
  "Get object by key and add it to black list. Ignore keys for that there are not objects in the storage."
  (awhen (getobj key 'product)
    (black-list.insert it timestamp)))

(defmethod black-list.insert ((product product) &optional (timestamp (get-universal-time)))
  "Make new instance & insert item to black list"
  (declare (integer timestamp))
  (setf (gethash (key product) black-list.*storage*)
        (black-list.%create-item product timestamp)))

;;; Deactivate items

;;--- TODO(wolforus@gmail.com): Rename & move funcs like this into product stuff file (or package)
(defmethod black-list.%deactive ((product product))
  "Deactive product. Make not avaible for order & for YML."
  (set-option product "Secret" "YML" "No")
  (setf (active product) nil))

(defmethod black-list.%deactive ((item black-list.item))
  "Deactive product item from black list"
  (black-list.%deactive (black-list.item-item item)))

(defun black-list.deactivate-all ()
  "Deactive all products from black list"
  (maphash #'(lambda (key item)
               (declare (ignore key))
               (black-list.%deactive item))
           black-list.*storage*))

(defun black-list.clear ()
  "Remove some items from black list"
  (maphash #'(lambda (key item)
               (awhen (black-list.item-item item)
                 (when (> (count-total it) (count-transit it))
                   (remhash key black-list.*storage*))))
           black-list.*storage*))

;;; Reports

(defmethod black-list.%print-item ((product product))
  (format nil "~A;~S;~A;~A;~A;~A;"
          (key product) (name-seo product)
          (aif (parent product)
               (name it)
               "")
          (siteprice product) (erp-class product)
          (- (count-total product) (count-transit product))))

(defmethod black-list.%print-item ((item black-list.item))
  (declare (black-list.item item))
  (format nil "~&~A~A;~%"
          (black-list.%print-item (black-list.item-item item))
          (time.encode.backup (black-list.item-add-ts item))))

(defun black-list.report (&optional (stream t))
  (format stream "~&артикул;название товара; название группы;цена;категория;на лев;дата добавления;~%")
  (maphash #'(lambda (key item)
               (declare (ignore key))
               (format stream "~A" (black-list.%print-item item)))
           black-list.*storage*))



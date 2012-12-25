;;;; Author: wolforus@gmail.com (Fedor Voronin)

;;;; Black-list items.

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

(defmethod black-list.insert ((key string) &optional (timestamp (get-universal-time)))
  "Get object by key and add it to black list. Ignore keys for that there are not objects in the storage."
  (awhen (getobj key 'product)
    (black-list.%create-item it timestamp)))

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
  (black-list.%deactive item))

(defun black-list.deactivate-all ()
  "Deactive all products from black list"
  (maphash #'(lambda (key item)
               (declare (ignore key))
               (black-list.%deactive item))
           black-list.*storage*))

;;; Reports

(defmethod black-list.%print-item ((product product))
  (format nil "~A;~S;~A;~A;~A;"
          (key product) (name-seo product)
          (aif (parent product)
               (name it)
               "")
          (siteprice product) (erp-class product)))

(defmethod black-list.%print-item ((item black-list.item))
  (declare (black-list.item item))
  (format nil "~&~A~A;~%"
          (black-list.%print-item (black-list.item-item item))
          (black-list.item-add-ts item)))

(defun black-list.report (&optional (stream t))
  (format stream "~&артикул;название товара; название группы;цена;категория;дата добавления;~%")
  (maphash #'(lambda (key item)
               (declare (ignore key))
               (format stream "~A" (black-list.%print-item item)))
           black-list.*storage*))


;;; LEGACY
;;---TODO (wolforus@gmail.com): REMOVE legacy & migrate code

(defvar *gateway.import-time* nil)
(defun gateway.restore-singles (dump-timestamp &optional (current-timestamp (get-universal-time)))
  "Load singles products witch came between dump-timestamp and current-timestamp"
  (declare (number dump-timestamp current-timestamp))
  (let* ((data))
    (labels ((@time (line) (subseq line 0 19))
             (@json (line) (subseq line 21))
             (@validp (line) (<= dump-timestamp (time.decode.backup (@time line))
                                current-timestamp)))
      (with-open-file (file (%gateway.singles-pathname))
        (loop
           :for line = (read-line file nil 'EOF)
           :until (eq line 'EOF)
           :when (@validp line)
           :do (let ((*gateway.import-time* (time.decode.backup (@time line))))
                 (setf data (json:decode-json-from-string (@json line)))
                 (%gateway.process-products-dump-data data)))))))

(defun t.%kill-bad-products ()
  (black-list.deactivate-all))

(defun t.%save-bad-product (product)
  (debug-slime-format "~A" product)
  (black-list.insert product *gateway.import-time*))

(defun black-list.%migrate ()
  (gateway.restore-singles (time.decode.backup "2012-10-16_12:01:23"))
  (maphash #'(lambda (k v)
               (declare (ignore v))
               (unless (gethash k *bad-products*)
                 (remhash k black-list.*storage*)))
           black-list.*storage*)
  (maphash #'(lambda (k v)
               (unless (gethash k black-list.*storage*)
                 (black-list.insert v)))
           *bad-products*)
  (hash-table-count black-list.*storage*)
  (gateway.load))

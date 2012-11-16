;;;; cart.lisp

(in-package #:eshop)

(defvar *order-id* nil)

(defun get-order-id ()
  "Generate pseudo-unique order number"
  (let ((current-order-id *order-id*)
        (order-id-pathname (merge-pathnames *path-order-id-file*
                                            (config.get-option :critical :path-to-conf))))
    (if *order-id*
        (progn
          (incf *order-id*)
          (with-open-file (file order-id-pathname
                                :direction :output
                                :if-exists :supersede
                                :external-format :utf-8)
            (format file "~a" *order-id*))
          current-order-id)
        ;;else
        (progn
          ;;если в файле шлак, то сбрасываем счетчик заказов до 1
          (setf *order-id*
                (handler-case
                    (parse-integer
                     (alexandria:read-file-into-string
                      order-id-pathname))
                  (SB-INT:SIMPLE-PARSE-ERROR () 1)
                  (SB-INT:SIMPLE-FILE-ERROR () 1)))
          (get-order-id)))))

;;проверка заказа на валидность
;;TODO сделать полную проверку
(defun order-valid-p (products)
  (not (null products)))

(defun save-order-text (file-name body)
  (when (config.get-option :start-options :release)
    (let ((filename (merge-pathnames (format nil "orders/~A.html" file-name)
                                     (config.get-option :paths :path-to-dropbox))))
      (with-open-file
          (stream filename :direction :output :if-exists :supersede)
        (format stream "~a" body)))))

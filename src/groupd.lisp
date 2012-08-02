;;;; groupd.lisp

(in-package #:eshop)

(defparameter *groupd.storage* (make-hash-table :test #'equal))
(defparameter *groupd.holiday.storage* (make-hash-table :test #'equal))

(defun groupd.line-processor (storage line)
  (let* ((words (sklonenie-get-words line))
         (skls (mapcar #'(lambda (w) (string-trim "#\""  w))
                       words))
         (key (car skls)))
    (setf (gethash key storage) skls)))

(defun groupd.restore ()
  (let ((t-storage (make-hash-table :test #'equal)))
    (xls.restore-from-xls
     (merge-pathnames "sale.xls" (config.get-option "PATHS" "path-to-main-page"))
     #'(lambda (line) (groupd.line-processor t-storage line))
     "groupd.restore")
    (when t-storage
      (setf *groupd.storage* t-storage))))

(defmethod groupd.is-groupd ((object product))
  (gethash (format nil "~a" (articul object)) *groupd.storage*))

(defun groupd.holiday.restore ()
  (let ((t-storage (make-hash-table :test #'equal)))
    (xls.restore-from-xls
     (merge-pathnames "holiday.xls" (config.get-option "PATHS" "path-to-main-page"))
     #'(lambda (line) (groupd.line-processor t-storage line))
     "groupd.holiday.restore")
    (when t-storage
      (setf *groupd.holiday.storage* t-storage))))

(defmethod groupd.holiday.is-groupd ((object product))
  (gethash (format nil "~a" (articul object)) *groupd.holiday.storage*))

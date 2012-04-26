;;;; groupd.lisp

(in-package #:eshop)

(defparameter *groupd.storage* (make-hash-table :test #'equal))
(defparameter *groupd.man.storage* (make-hash-table :test #'equal))
(defparameter *groupd.woman.storage* (make-hash-table :test #'equal))
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

(defun groupd.man.restore ()
	(let ((t-storage (make-hash-table :test #'equal)))
		(xls.restore-from-xls
		 (merge-pathnames "man_sale.xls" (config.get-option "PATHS" "path-to-main-page"))
		 #'(lambda (line) (groupd.line-processor t-storage line))
		 "groupd.man.restore")
		(when t-storage
			(setf *groupd.man.storage* t-storage))))

(defmethod groupd.man.is-groupd ((object product))
  (gethash (format nil "~a" (articul object)) *groupd.man.storage*))

(defun groupd.woman.restore ()
	(let ((t-storage (make-hash-table :test #'equal)))
		(xls.restore-from-xls
		 (merge-pathnames "woman_sale.xls" (config.get-option "PATHS" "path-to-main-page"))
		 #'(lambda (line) (groupd.line-processor t-storage line))
		 "groupd.woman.restore")
		(when t-storage
			(setf *groupd.woman.storage* t-storage))))

(defmethod groupd.woman.is-groupd ((object product))
  (gethash (format nil "~a" (articul object)) *groupd.woman.storage*))

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

(in-package #:eshop)

(defparameter *groupd.storage* (make-hash-table :test #'equal))
(defparameter *groupd.man.storage* (make-hash-table :test #'equal))
(defparameter *groupd.woman.storage* (make-hash-table :test #'equal))

(defun groupd.restore ()
  (let ((t-storage))
    (let ((*groupd.storage* (make-hash-table :test #'equal)))
      (groupd.load *groupd.storage* "sale.xls")
      (setf t-storage *groupd.storage*))
    (setf  *groupd.storage* t-storage)
    (log5:log-for info "DONE groupd.restore")))

(defun groupd.load (storage filename)
  (let ((header-line)
        (proc (sb-ext:run-program
               "/usr/bin/xls2csv"
               (list "-q3" (format nil "~a/mainPage/~a" *path-to-dropbox* filename)) :wait nil :output :stream)))
    (with-open-stream (stream (sb-ext:process-output proc))
      (setf header-line (read-line stream nil))
      (print header-line)
      (loop
         :for line = (read-line stream nil)
         :until (or (null line)
                    (string= "" (string-trim "#\," line)))
         :do (let* ((words (sklonenie-get-words line))
                    (skls (mapcar #'(lambda (w) (string-trim "#\""  w))
                             words))
                    (key (car skls)))
               (setf (gethash key storage) skls))))))


(defmethod groupd.is-groupd ((object product))
  (gethash (format nil "~a" (articul object)) *groupd.storage*))

(defun groupd.man.restore ()
  (let ((t-storage))
    (let ((*groupd.man.storage* (make-hash-table :test #'equal)))
      (groupd.load *groupd.man.storage* "man_sale.xls")
      (setf t-storage *groupd.man.storage*))
    (setf  *groupd.man.storage* t-storage)
    (log5:log-for info "DONE groupd.man.restore")))

(defmethod groupd.man.is-groupd ((object product))
  (gethash (format nil "~a" (articul object)) *groupd.man.storage*))

(defun groupd.woman.restore ()
  (let ((t-storage))
    (let ((*groupd.woman.storage* (make-hash-table :test #'equal)))
      (groupd.load *groupd.woman.storage* "woman_sale.xls")
      (setf t-storage *groupd.woman.storage*))
    (setf  *groupd.woman.storage* t-storage)
    (log5:log-for info "DONE groupd.woman.restore")))

(defmethod groupd.woman.is-groupd ((object product))
  (gethash (format nil "~a" (articul object)) *groupd.woman.storage*))

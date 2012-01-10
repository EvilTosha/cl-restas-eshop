(in-package #:eshop)

(defparameter *groupd.storage* (make-hash-table :test #'equal))

(defun groupd.restore ()
  (let ((t-storage))
    ;; (wlog "Start (main-page-restore):")
    (let ((*groupd.storage* (make-hash-table :test #'equal)))
      (groupd.load *groupd.storage* "sale.xls")
      (setf t-storage *groupd.storage*))
    (setf  *groupd.storage* t-storage)
    (wlog "DONE")))

(defun groupd.load (storage filename)
  (let (
        ;; (num 0)
        (header-line)
        (proc (sb-ext:run-program
               "/usr/bin/xls2csv"
               (list "-q3" (format nil "~a/mainPage/~a" *path-to-dropbox* filename)) :wait nil :output :stream)))
    (with-open-stream (stream (sb-ext:process-output proc))
      (setf header-line (read-line stream nil))
      ;; (print header-line)
      (loop
         :for line = (read-line stream nil)
         :until (or (null line)
                    (string= "" (string-trim "#\," line)))
         :do (let* ((words (sklonenie-get-words line))
                    (skls (mapcar #'(lambda (w) (string-trim "#\""  w))
                             words))
                    (key (car skls)))
               ;; (wlog line)
               ;; (incf num)
               (setf (gethash key storage) skls)
               ;; (format t "~&~a: ~{~a~^,~}" key skls)
               )))))



(defmethod groupd.is-groupd ((object product))
  ;; (wlog (articul object))
  (gethash (format nil "~a" (articul object)) *groupd.storage*))

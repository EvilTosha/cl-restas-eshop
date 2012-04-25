;;;; cartrige.lisp

(in-package #:eshop)

(defclass printer ()
	((original-cartriges :initarg :original-cartriges :initform nil   :accessor original-cartriges)
	 (other-cartriges :initarg :other-cartriges :initform nil   :accessor other-cartriges)
	 (vendor    :initarg :vendor    :initform ""    :accessor vendor)
	 (name      :initarg :name      :initform ""    :accessor name)
	 (printer-type      :initarg :printer-type      :initform ""    :accessor printer-type)))

(defparameter *printer-storage* (make-hash-table :test #'equal))

(defun cartrige.add-printer (articul original-cartriges other-cartriges
														 &optional vendor name printer-type)
	(let ((printer (gethash articul (storage *global-storage*))))
		(setf (gethash articul *printer-storage*)
					(make-instance 'printer
												 :original-cartriges original-cartriges
												 :other-cartriges other-cartriges
												 :vendor (if vendor
																		 vendor
																		 (vendor printer))
												 :name (if name
																	 name
																	 (name printer))
												 :printer-type printer-type))))

(defun cartrige.add-cartrige (printer-articul cartrige-articul &optional (cartrige-type :other))
	(if (eq cartrige-type :original)
			(pushnew cartrige-articul (original-cartriges (gethash printer-articul *printer-storage*)))
			(pushnew cartrige-articul (other-cartriges (gethash printer-articul *printer-storage*)))))


(defun cartrige.get-cartriges-by-model (articul &optional (cartrige-type :all))
	(let ((printer (gethash articul *printer-storage*))
				(global-printer (gethash articul (storage *global-storage*))))
		(when (and printer global-printer)
			(case cartrige-type
				(:all (append (original-cartriges printer) (other-cartriges printer)))
				(:original (original-cartriges printer))
				(:other (other-cartriges printer))))))

(defun cartrige.get-printers-by-params (&key vendor printer-type)
	(let ((result nil))
		(maphash #'(lambda (k v)
								 (when (and (or (null vendor)
																(string= vendor (vendor v)))
														(or (null printer-type)
																(string= printer-type (printer-type v))))
									 (push k result)))
						 *printer-storage*)
		result))

(defun cartrige.restore ()
  (let ((t-storage))
    (let ((*printer-storage* (make-hash-table :test #'equal)))
      (groupd.load *printer-storage* "printyrikartridji.xls")
      (setf t-storage *printer-storage*))
    (setf  *printer-storage* t-storage)
    (log5:log-for info "DONE cartrige.restore")))

(defun cartrige.load (storage filename)
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

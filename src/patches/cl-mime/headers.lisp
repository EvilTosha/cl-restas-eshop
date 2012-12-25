(in-package :mime)

(defmethod get-header ((mime-obj mime) (header (eql :content-type)))
  (cons header
	(format nil "~A/~A~A~A"
		(content-type mime-obj)
		(content-subtype mime-obj)
		;; Required parameters for particular MIME types
		(typecase mime-obj
		  (text-mime
		   (format nil "; charset=~A"
			   (charset mime-obj)))
		  (multipart-mime
		   (format nil "; boundary=\"~A\";~% charset=windows-1251"
			   (boundary mime-obj)))
		  (otherwise "; charset=windows-1251"))
		;; All remaining parameters defined by the user
		(format nil "~{~{;~%~5,5T~A=\"~A\"~}~}"
			(mapcar
			 (lambda (parm-pair)
			   (cons (string-downcase (symbol-name (car parm-pair)))
				 (cdr parm-pair)))
			 (content-type-parameters mime-obj))))))

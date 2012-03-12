(in-package #:eshop)

;;; Backup functions and macros'


(defmacro backup.make-serialize-method (name class-fields)
	"Macros for creating serialize methods"
  `(list
    (defmethod backup.serialize-entity ((object ,name))
      (format nil "{~{~a~^,~}}"
              (remove-if #'null
                         ,(cons
													 `list
													 (mapcar #'(lambda (field)
																			 `(let* ((field-value (,(getf field :name) object))
																							 (encoded-value (when field-value
																																(,(intern (string-upcase
																																					 (format nil "object-fields.~a-field-serialize" (getf field :type))))
																																	field-value))))
																					(when (and field-value
																										 (string/= (format nil "~a" field-value) "")
																										 (not (equal field-value ,(getf field :initform)))
																										 encoded-value)
																					 (format nil "~a:~a"
																									 (encode-json-to-string (quote ,(getf field :name)))
																									 encoded-value))))
													 (remove-if-not #'(lambda (field)
																							(getf field :serialize))
																					class-fields))))))
    (defmethod backup.serialize-to-file ((object ,name) pathname)
      (with-open-file (file pathname
                            :direction :output
                            :if-exists :supersede
                            :external-format :utf-8)
				(format file "~a" (backup.serialize-entity object))))))

(defun backup.serialize-list-to-file (object-list filepath)
  (let ((total 0))
    (with-open-file (file filepath
                          :direction :output
                          :if-exists :supersede
													:if-does-not-exist :create
                          :external-format :utf-8)
			(mapcar #'(lambda (object)
									(format file "~a~%" (backup.serialize-entity object))
									(incf total))
							object-list))
    (wlog (format nil "Total serialized: ~a" total))))



(defun backup.serialize-all (&key (backup-dir (format nil "~aeshop-logs/backups/" (user-homedir-pathname)))
														 product-filename group-filename (push-to-dropbox t))
	"Serializing all products & groups to given files in given folder. If no filenames passed, it makes files with type-date-time.bkp template"
	(let* ((date-time (time.encode.backup-filename))
				 (dropbox-backup-path (format nil "~a/eshop-backups/" *path-to-dropbox*))
				 (product-path (pathname
												(if product-filename
														(format nil "~aproducts/~a" backup-dir product-filename)
														(format nil "~aproducts/prods-~a.bkp" backup-dir date-time))))
				 (group-path (pathname
											(if group-filename
													(format nil "~agroups/~a" backup-dir group-filename)
													(format nil "~agroups/group-~a.bkp" backup-dir date-time)))))
		(ensure-directories-exist product-path)
		(ensure-directories-exist group-path)
		(wlog "start products serializing")
		(backup.serialize-list-to-file (storage.get-products-list) product-path)
		(wlog "start groups serializing")
		(backup.serialize-list-to-file (storage.get-groups-list) group-path)
		;; copying to Dropbox
		(when push-to-dropbox
			(ensure-directories-exist dropbox-backup-path)
			(cl-fad:copy-file product-path (format nil "~aproducts.bkp" dropbox-backup-path) :overwrite t)
			(cl-fad:copy-file group-path   (format nil "~agroups.bkp" dropbox-backup-path) :overwrite t))))


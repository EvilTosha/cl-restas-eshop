;;;;backup.lisp

(in-package #:eshop)

;;; Backup functions and macros

(defmacro backup.define-serialize-method (name class-slots)
  "Macros for creating serialize method"
  `(defmethod backup.serialize-entity ((object ,name))
     (format nil "{~{~a~^,~}}"
             (remove-if #'null
                        (list
                         ,@(mapcar #'(lambda (slot)
                                       `(let* ((slot-value (,(getf slot :name) object))
                                               (encoded-value (when slot-value
                                                                (slots.%encode-to-string
                                                                 ',(getf slot :type)
                                                                 slot-value))))
                                          (when (and slot-value
                                                     (string/= (format nil "~a" slot-value) "")
                                                     ;; FIXME: it's not correct to use such getter
                                                     ;; for initform here
                                                     (not (equal slot-value ,(getf slot :initform)))
                                                     encoded-value)
                                            (format nil "~a:~a"
                                                    (encode-json-to-string ',(getf slot :name))
                                                    (encode-json-to-string encoded-value)))))
                                   (remove-if-not #'(lambda (slot)
                                                      (getf slot :serialize))
                                                  class-slots)))))))


(defun backup.serialize-storage-to-file (type filepath)
  "Serialize storage of given type to file."
  (declare (symbol type) (pathname filepath))
  (with-open-file (file filepath
                        :direction :output
                        :if-exists :supersede
                        :if-does-not-exist :create
                        :external-format :utf-8)
    (process-storage #'(lambda (obj)
                         (format file "~A~%" (backup.serialize-entity obj)))
                     type))
  (log5:log-for info "Total serialized: ~a" (count-storage type)))


(defun backup.serialize-all (&key (backup-dir (config.get-option "PATHS" "path-to-backups"))
                             (make-copy (config.get-option "START_OPTIONS" "release"))
                             (copy-path
                              (config.get-option "CRITICAL" "path-to-dropbox-backup")))
  "Serializing all products & groups & vendors to given files in given folder. If no filenames passed, it makes files with type-date-time.bkp template"
  (let* ((date-time (time.encode.backup-filename)))
    (maphash
     #'(lambda (class properties)
         (when (getf properties :serialize)
           (let ((path (merge-pathnames
                        (format nil "~(~A~)/~(~:*~A~)-~A.bkp"
                                class date-time) backup-dir)))
             (ensure-directories-exist path)
             (log5:log-for info "Start ~(~A~) serialize to ~A" class path)
             (backup.serialize-storage-to-file class path)
             (when make-copy
               (ensure-directories-exist copy-path)
               (cl-fad:copy-file
                path (merge-pathnames (format nil "~(~A~).bkp" class) copy-path)
                :overwrite t)))))
     *classes*)))

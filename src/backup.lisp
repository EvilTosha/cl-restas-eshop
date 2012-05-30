;;;;backup.lisp

(in-package #:eshop)

;;; Backup functions and macros

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

(defun backup.serialize-collection-to-file (object-collection filepath)
  "Serialize collection (list or hash-table) to file."
  (with-open-file (file filepath
                        :direction :output
                        :if-exists :supersede
                        :if-does-not-exist :create
                        :external-format :utf-8)
    (servo.iterate #'(lambda (obj)
                       (format file "~A~%" (backup.serialize-entity obj)))
                   object-collection))
    (log5:log-for info "Total serialized: ~a" (servo.collection-count object-collection)))



(defun backup.serialize-all (&key (backup-dir (config.get-option "PATHS" "path-to-backups"))
                                  product-filename
                                  group-filename
                                  vendor-filename
                                  (push-to-dropbox (config.get-option "START_OPTIONS" "release")))
  "Serializing all products & groups & vendors to given files in given folder. If no filenames passed, it makes files with type-date-time.bkp template"
  (let* ((date-time (time.encode.backup-filename))
         (product-path (merge-pathnames
                        (if product-filename
                            (format nil "products/~a" product-filename)
                            (format nil "products/prods-~a.bkp" date-time))
                        backup-dir))
         (group-path (merge-pathnames
                      (if group-filename
                          (format nil "groups/~a" group-filename)
                          (format nil "groups/group-~a.bkp" date-time))
                      backup-dir))
         (vendor-path (merge-pathnames
                       (if vendor-filename
                           (format nil "vendors/~a" vendor-filename)
                           (format nil "vendors/vendors-~a.bkp" date-time))
                       backup-dir)))
    (ensure-directories-exist product-path)
    (ensure-directories-exist group-path)
    (ensure-directories-exist vendor-path)
    (log5:log-for info "Start products serializing to ~a" product-path)
    (backup.serialize-collection-to-file (storage.get-products-list) product-path)
    (log5:log-for info "Start groups serializing to ~a" group-path)
    (backup.serialize-collection-to-file (storage.get-groups-list) group-path)
    (log5:log-for info "Start vendor serializing to ~a" vendor-path)
    (backup.serialize-collection-to-file *vendor-storage* vendor-path)
    ;; copying to Dropbox
    (when push-to-dropbox
      (let ((dropbox-backup-path (config.get-option "CRITICAL" "path-to-dropbox-backup")))
        (ensure-directories-exist dropbox-backup-path)
        (cl-fad:copy-file product-path (merge-pathnames "products.bkp" dropbox-backup-path) :overwrite t)
        (cl-fad:copy-file group-path (merge-pathnames "groups.bkp" dropbox-backup-path) :overwrite t)
        (cl-fad:copy-file vendor-path (merge-pathnames "vendors.bkp" dropbox-backup-path) :overwrite t)))))


(defun backup.last-product-backup-pathname ()
  (merge-pathnames "products.bkp" (config.get-option "PATHS" "path-to-last-backup")))

(defun backup.last-group-backup-pathname ()
  (merge-pathnames "groups.bkp" (config.get-option "PATHS" "path-to-last-backup")))

(defun backup.last-filter-backup-pathname ()
  (merge-pathnames "filters.bkp" (config.get-option "PATHS" "path-to-last-backup")))

(defun backup.last-vendor-backup-pathname ()
  (merge-pathnames "vendors.bkp" (config.get-option "PATHS" "path-to-last-backup")))

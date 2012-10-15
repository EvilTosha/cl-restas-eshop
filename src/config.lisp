;;;; config.lisp

(in-package #:eshop)

(defparameter *eshop-config* (make-instance 'py-configparser:config))

(defun config.get-option (section option)
  ;; for correct work not only with strings
  (py-configparser:get-option *eshop-config* section option :expand nil))

(defun config.set-option (section option value)
  (py-configparser:set-option *eshop-config* section option value))

(defun config.has-option-p (section option)
  (py-configparser:has-option-p *eshop-config* section option))

(defun config.path-processing (path-string &optional check-existance)
  "Check if file at path-string exists, and convert to pathname.
   If path-string starts with /, function assumes that it's global name, otherwise path from homedir"
  (let ((path (pathname (if (char= (char path-string 0) #\/)
                            path-string
                            (format nil "~a~a" (user-homedir-pathname) path-string)))))
    (when (and check-existance (not (file-exists-p path)))
      (error (format nil "File ~a doesn't exist" path)))
    path))

(defun config.bool-processing (bool-string)
  (string-case bool-string
    ("1" t)
    ("0" nil)
    (t (error "Wrong bool argument, must be 1/0"))))

(defun config.int-processing (int-string)
  (parse-integer int-string))

(defun config.string-processing (string)
  string)

(defun config.config-option-processing (section option type)
  (if (config.has-option-p section option)
      (progn
        (format *standard-output* "Processing ~a/~a option..." section option)
        (config.set-option section option
                           (let ((option-value
                                  (config.get-option section option)))
                             (string-case type
                               ("path" (config.path-processing option-value))
                               ("bool" (config.bool-processing option-value))
                               ("int" (config.int-processing option-value))
                               ("string" (config.string-processing option-value)))))
        (format *standard-output* "   ~a~%" (config.get-option section option)))
      ;; option doesn't exist
      (error (format nil "Option ~a/~a doesn't exist" section option))))


(defun config.parse-config (&optional (path-to-config (sb-unix::posix-getenv "CONFIG_PATH")))
  (with-open-file (file path-to-config
                        :direction :input
                        :if-does-not-exist :error)
    (py-configparser:read-stream *eshop-config* file))
  ;; processing options here
  ;; START_OPTIONS section
  (config.config-option-processing "START_OPTIONS" "release" "bool")
  (config.config-option-processing "START_OPTIONS" "dbg-on" "bool")
  (config.config-option-processing "START_OPTIONS" "catch-errors" "bool")
  (config.config-option-processing "START_OPTIONS" "server-port" "int")
  (config.config-option-processing "START_OPTIONS" "load-storage" "bool")
  (config.config-option-processing "START_OPTIONS" "load-xls" "bool")
  (config.config-option-processing "START_OPTIONS" "load-content" "bool")
  (config.config-option-processing "START_OPTIONS" "run-cron-jobs" "bool")
  (config.config-option-processing "START_OPTIONS" "make-marketing-filters" "bool")
  ;; PATHS section
  (config.config-option-processing "PATHS" "path-to-last-backup" "path")
  (config.config-option-processing "PATHS" "path-to-templates" "path")
  (config.config-option-processing "PATHS" "path-to-articles" "path")
  (config.config-option-processing "PATHS" "path-to-pics" "path")
  (config.config-option-processing "PATHS" "path-to-static-pages" "path")
  (config.config-option-processing "PATHS" "path-to-dropbox" "path")
  (config.config-option-processing "PATHS" "path-to-logs" "path")
  (config.config-option-processing "PATHS" "path-to-gateway" "path")
  (config.config-option-processing "PATHS" "path-to-backups" "path")
  (config.config-option-processing "PATHS" "path-to-big-images-backup" "path")
  (config.config-option-processing "PATHS" "path-to-main-page" "path")
  (config.config-option-processing "PATHS" "path-to-seo" "path")
  ;; CRITICAL section
  ;; these paths should be different on developer and release servers
  (config.config-option-processing "CRITICAL" "path-to-conf" "path")
  (config.config-option-processing "CRITICAL" "path-to-sitemap" "path")
  (config.config-option-processing "CRITICAL" "path-to-dropbox-backup" "path")
  ;; OTHER_OPTIONS section
  (config.config-option-processing "OTHER_OPTIONS" "pics-cache-ttl" "int")
  t)


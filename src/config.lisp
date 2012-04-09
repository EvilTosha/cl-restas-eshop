(in-package #:eshop)

(defun config.path-processing (path-string &optional check-existance)
	"Check if file at path-string exists, and convert to pathname.
   If path-string starts with /, function assumes that it's global name, otherwise path from homedir"
	(let ((path (pathname (if (char= (char path-string 0) #\/)
														path-string
														(format nil "~a~a" (user-homedir-pathname) path-string)))))
		(when (and check-existance (not (file-exists-p path)))
			(error (format nil "File ~a doesn't exist" path)))
		(ensure-directories-exist path)
		path))

(defun config.bool-processing (bool-string)
	(cond
		((string= "1" bool-string) t)
		((string= "0" bool-string) nil)
		(t (error "Wrong bool argument, must be 1/0"))))

(defun config.int-processing (int-string)
	(parse-integer int-string))

(defparameter *eshop-config* (make-instance 'py-configparser:config))

(defun config.config-option-processing (section option type)
	(if (py-configparser:has-option-p *eshop-config* section option)
			(progn
				(format t "Processing ~a/~a option..." section option)
				(py-configparser:set-option *eshop-config* section option
																		(let ((option-value
																					 (py-configparser:get-option *eshop-config* section option)))
																		(cond
																			((string= type "path") (config.path-processing option-value))
																			((string= type "bool") (config.bool-processing option-value))
																			((string= type "int") (config.int-processing option-value)))))
				(format t "   ~a~%" (py-configparser:get-option *eshop-config* section option)))
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
	(config.config-option-processing "START_OPTIONS" "swank-port" "int")
	(config.config-option-processing "START_OPTIONS" "server-port" "int")
	;; PATHS section
	(config.config-option-processing "PATHS" "path-to-src" "path")
	(config.config-option-processing "PATHS" "path-to-last-backup" "path")
	(config.config-option-processing "PATHS" "path-to-templates" "path")
	(config.config-option-processing "PATHS" "path-to-articles" "path")
	(config.config-option-processing "PATHS" "path-to-dropbox" "path")
	(config.config-option-processing "PATHS" "path-to-logs" "path")
	(config.config-option-processing "PATHS" "path-to-backups" "path")
	t)

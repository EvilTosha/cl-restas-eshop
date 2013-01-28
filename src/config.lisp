;;;; config.lisp

(in-package #:eshop)

;; default-config, which will be changed during parsing of actual config
;; TODO: add validation options (at least for paths)
(defparameter *eshop-config*
  (list
   ;; START-OPTIONS section
   :start-options
   (list :release                (list :value nil  :type :bool)
         :dbg-on                 (list :value t    :type :bool)
         ;; catch-errors shold be t on release
         :catch-errors           (list :value nil  :type :bool)
         :server-port            (list :value 8080 :type :int)
         :load-storage           (list :value t    :type :bool)
         :load-xls               (list :value t    :type :bool)
         :load-content           (list :value t    :type :bool)
         :run-cron-jobs          (list :value nil  :type :bool)
         ;; won't work without load storage
         :make-marketing-filters (list :value nil  :type :bool)
         )
   ;; PATHS section
   :paths
   (list :path-to-dropbox              (list :value #P"Dropbox/"                      :type :path)
         :path-to-last-backup          (list :value #P"eshop-dev/last-backup/"        :type :path)
         :path-to-templates            (list :value #P"Dropbox/httpls/release/"       :type :path)
         :path-to-articles             (list :value #P"Dropbox/content/articles/"     :type :path)
         :path-to-static-pages         (list :value #P"Dropbox/content/static-pages/" :type :path)
         :path-to-pics                 (list :value #P"htpics1/"                      :type :path)
         :path-to-logs                 (list :value #P"eshop-logs/"                   :type :path)
         :path-to-gateway              (list :value #P"eshop-logs/gateway/"           :type :path)
         :path-to-backups              (list :value #P"eshop-dev/backups/"            :type :path)
         :path-to-big-images-backup    (list :value #P"source-big-images-bkps/"       :type :path)
         :path-to-main-page            (list :value #P"Dropbox/mainPage/"             :type :path)
         :path-to-seo                  (list :value #P"Dropbox/seo/"                  :type :path)
         :path-to-reports              (list :value #P"Dropbox/reports/"              :type :path)
         :path-to-xls                  (list :value #P"Dropbox/xls/"                  :type :path)
         )
   ;; CRITICAL section, options should be different for dev and release servers
   :critical
   (list :path-to-conf                 (list :value #P"eshop-dev/htconf/"             :type :path)
         :path-to-order-id-file        (list :value #P"eshop-dev/htconf/dev-order-id.txt"
                                                                                      :type :path)
         :path-to-sitemap              (list :value #P"eshop-dev/htconf/"             :type :path)
         :path-to-dropbox-backup       (list :value #P"Dropbox/eshop-backups/"        :type :path)
         :send-emails                  (list :value nil                               :type :bool)
         :gateway-warn-emails          (list :value (list "Supplers@alpha-pc.com"
                                                          "web_design@alpha-pc.com"
                                                          "wolforus@gmail.com"
                                                          "slamly@gmail.com")         :type :string-list)
         :xls-warn-emails              (list :value (list "wolforus@gmail.com"
                                                          "web_design@alpha-pc.com")  :type :string-list)
         :order-emails                 (list :value (list "internetorder@alpha-pc.com"
                                                          "shop@320-8080.ru"
                                                          "zakaz320@yandex.ru"
                                                          "slamly@gmail.com"
                                                          "wolforus@gmail.com")       :type :string-list)
         :from-email                   (list :value "shop@320-8080.ru"                :type :string)
         )
   ;; OTHER-OPTIONS section
   :other-options (list
                   ;; 24 hours in seconds
                   :pics-cache-ttl     (list :value 86400 :type :int)))
"Variable for storing config options")

(defun config.parse-config (&optional (path-to-config (sb-unix::posix-getenv "CONFIG_PATH")))
  "Reads specified file and process options"
  (declare ((or string pathname) path-to-config))
  (with-open-file (file path-to-config
                        :direction :input
                        :if-does-not-exist :error)
    ;; ??: probably use other options (not in :sections) while parsing
    (let ((conf (getf (read file) :sections)))
      ;; process and update default config
      (alexandria:doplist (section-name section conf)
        (alexandria:doplist (option-name option-value section)
          (if (not (config.option-exists-p section-name option-name))
              ;; create option (and probably section
              (config.set-option section-name option-name option-value)
              ;; else, validate, process, update
              (let* ((processing-options (config.%get-option-data section-name option-name))
                     (value option-value)
                     (type (getf processing-options :type)))
                ;; remove non-options
                (remf processing-options :value)
                (remf processing-options :type)
                (if (not (apply #'config.%validate-option type value processing-options))
                    (error "Invalid option value for option ~A/~A" section-name option-name)
                    (config.set-option section-name option-name
                                       (apply #'config.%process-option type value processing-options))))))))))

;; TODO: make setfable
(defun config.%get-option-data (section option)
  "Retreive option data (including type and processing/validating options) from given section of config.
If option or section isn't found, signals error.
Return format is plist"
  (declare (keyword section option))
  (let* ((section-data (getf *eshop-config* section 'section-not-found))
         (option-data (if (equal 'section-not-found section-data)
                          (error "Section ~A not found in config" section)
                          (getf section-data option 'option-not-found))))
    (if (equal 'option-not-found option-data)
        (error "Option ~A/~A not found in config" section option)
        option-data)))

(defun config.get-option (section option)
  "Retreive option from given section of config. If option or section isn't found, signals error"
  (declare (keyword section option))
  (getf (config.%get-option-data section option) :value))

;; TODO: write (setf config.get-option) function
;; TODO: don't duplicate code from config.get-option
;; TODO: create section/option if they're not exist
(defun config.set-option (section option value)
  "Sets specified option to new value. If option or section isn't found, signals error"
  (declare (keyword section option))
  (let* ((section-data (getf *eshop-config* section 'section-not-found))
         (option-data (if (equal 'section-not-found section-data)
                          (error "Section ~A not found in config" section)
                          (getf section-data option 'option-not-found))))
    (if (equal 'option-not-found option-data)
        (error "Option ~A/~A not found in config" section option)
        (setf (getf option-data :value) value))))

;; TODO: don't duplicate code from config.get-option
(defun config.option-exists-p (section option)
  "Checks whether specified option exists in config."
  (declare (keyword section option))
  (let* ((section-data (getf *eshop-config* section 'section-not-found))
         (option-data (unless (equal 'section-not-found section-data)
                        (getf section-data option 'option-not-found))))
    (and (not (equal 'section-not-found section-data))
         (not (equal 'option-not-found option-data)))))

(defgeneric config.%validate-option (type option &key)
  (:documentation "Validates specified option with specified type rules")
  (:method (type option &key &allow-other-keys)
    t))

(defmethod config.%validate-option ((type (eql :path)) option &key)
  (pathnamep option))

(defmethod config.%validate-option ((type (eql :bool)) option &key)
  (typep option 'boolean))

(defmethod config.%validate-option ((type (eql :int)) option &key)
  (integerp option))

(defmethod config.%validate-option ((type (eql :string)) option &key)
  (stringp option))

(defmethod config.%validate-option ((type (eql :list)) option &key)
  (listp option))

(defmethod config.%validate-option ((type (eql :string-list)) option &key)
  (and (listp option) (every #'stringp option)))

(defgeneric config.%process-option (type option &key)
  (:documentation "Process (and possibly change, e.g. merge pathname with default location) option. Returns processed value.")
  (:method (type option &key &allow-other-keys) #| do nothing with option by default |# option))

(defmethod config.%process-option ((type (eql :path)) option &key check-existence)
  "Checks whether option is valid pathname. If check-existence provided, checks whether specified path to file/folder exists,
if not, signals error"
  (declare (boolean check-existence))
  (let ((option (merge-pathnames option (user-homedir-pathname))))
    (when (and check-existence (not (file-exists-p option)))
      (error (format nil "File ~A doesn't exist" option)))
    option))

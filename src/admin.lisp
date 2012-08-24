;;;; admin.lisp

(in-package #:eshop)

(defvar *admin-test-request*)
(defvar *admin-test-session*)

(restas:define-route admin-testtest-route ("/administration-super-panel/testtest")
  (setf *admin-test-request* hunchentoot:*request*)
  (setf *admin-test-session* hunchentoot:*session*)
  "TEST")

;; ADMIN ROUTE
(restas:define-route admin-route ("/administration-super-panel")
  (show-admin-page))

(restas:define-route admin/-route ("/administration-super-panel/")
  (show-admin-page))

(restas:define-route admin-actions-key-route ("/administration-super-panel/actions" :method :post)
  (show-admin-page "actions"))

(restas:define-route admin-edit-key-route ("/administration-super-panel/edit" :method :post)
  (show-admin-page "edit"))

(restas:define-route admin-make-key-route ("/administration-super-panel/make" :method :post)
  (show-admin-page "make"))

(restas:define-route admin-pics-route ("/administration-super-panel/pics" :method :post)
  (show-admin-page "pics"))

(restas:define-route admin-templates-route ("/administration-super-panel/templates" :method :post)
  (show-admin-page "templates"))

(restas:define-route admin-backup1-route ("/administration-super-panel/makebackup")
  (show-admin-page "backup"))

(restas:define-route admin-backup-route ("/administration-super-panel/makebackup" :method :post)
  (show-admin-page "backup"))

(restas:define-route admin-cron-route ("/administration-super-panel/cron-jobs" :method :post)
  (show-admin-page "cron-jobs"))

(restas:define-route admin-parenting-key-route ("/administration-super-panel/parenting" :method :post)
  (show-admin-page "parenting"))

(restas:define-route admin-key-route ("/administration-super-panel/:key")
  (show-admin-page key))

(restas:define-route admin-filter-create ("administration-super-panel/filter-create" :method :get)
  (string-case (hunchentoot:get-parameter "get")
    ("filter-types"
     ;; FIXME: use json encode, not format
     (format nil "[~{~A~^,~}]" (mapcar #'encode-json-plist-to-string
                                       (filters.get-basics-types))))
    ("fields"
     (format nil "[~{~A~^,~}]" (mapcar #'encode-json-plist-to-string
                                       (filters.get-basic-fields (hunchentoot:get-parameter "filter-type")))))))

(restas:define-route admin-filter-create-post ("administration-super-panel/filter-create" :method :post)
  nil)

(restas:define-route admin-edit-slot-route ("administration-super-panel/edit-slot" :method :post)
  (let ((object (getobj (hunchentoot:post-parameter "key")))
        (slot (anything-to-symbol (hunchentoot:post-parameter "slot")))
        (value (hunchentoot:post-parameter "value")))
    (if object
        (handler-case
            (progn
              (setf (slot-value object slot)
                    (slots.%get-data (slot-type (type-of object) slot) value))
              ;; FIXME: bad code
              (when (and (groupp object) (equal slot 'raw-fullfilter))
                (setf (fullfilter object) (decode-fullfilter (raw-fullfilter object))))
              ;; return value
              (encode-json-plist-to-string (list :success t :msg "Success")))
          (error (e) (encode-json-plist-to-string (list :success nil :msg (format nil "Error: ~A" e)))))
        ;; else
        (encode-json-plist-to-string (list :success nil :msg "Error: Object doesn't exist")))))


(defun admin-compile-templates ()
  (servo.compile-soy "admin.soy"
                     "class_forms.soy"))

(defun admin-update ()
  "Updates templates"
  (admin-compile-templates))

(defun admin.get-info ()
  (list (format nil "~{~a<br>~}" (mapcar #'(lambda (v) (sb-thread:thread-name v)) (sb-thread:list-all-threads)))
        (regex-replace-all "\\n" (with-output-to-string (*standard-output*) (room)) "<br>")))


(defun admin.edit-content (&optional post-data)
  (let* ((key (getf (request-get-plist) :key))
         (item (getobj key))
         (item-fields (when item (class-core.make-fields item))))
    (when (and item post-data)
      (setf post-data (admin.post-data-preprocessing (servo.plist-to-unique post-data)))
      (class-core.edit-fields item post-data)
      ;; need to fix
      (when (and (groupp item) (getf post-data :fullfilter))
        (setf (fullfilter item) (getf post-data :fullfilter)))
      (slots.product-groups-fix item)
      (setf item-fields (class-core.make-fields item)))
    (if item
        (soy.class_forms:formwindow
         (list :output (format nil "~a" post-data)
               :key key
               :fields item-fields
               :target "edit"))
        "not found")))

(defun admin.make-content (post-data)
  (let* ((key (getf (request-get-plist) :key))
         (type (getf (request-get-plist) :type))
         (item (getobj key)))
    (if item
        ;;if item exist in storage, redirect to edit page (but url still .../make?...)
        (admin.edit-content post-data)
        ;;else
        (if post-data
            (progn
              (string-case type
                ;; TODO: fix for all classes
                ("product"
                 (setf item (make-instance 'product
                                           :articul (parse-integer key))))
                ("group"
                 (setf item (make-instance 'group)))
                ("filter"
                 (setf item (make-instance 'filter)))
                (t "Unknown type"))
              (setf (key item) key)
              (if (equal type "product")
                  (setf (date-created item) (get-universal-time)
                        (date-modified item) (get-universal-time)))
              (setf post-data (admin.post-data-preprocessing (servo.plist-to-unique post-data)))
              (class-core.edit-fields item post-data)
              ;;doesn't work with filters
              (slots.product-groups-fix item)
              (setobj (key item) item) ;;adding item into storage
              (admin.edit-content))
            ;;else (post-data is nil)
            (let ((empty-item (get-instance type)))
              (setf (key empty-item) key)
              (if (productp empty-item)
                  (setf (articul empty-item) (parse-integer key)))
              (soy.class_forms:formwindow
               (list :key key
                     :type type
                     :fields (class-core.make-fields empty-item)
                     :target "make")))))))

(defun admin.pics-deleting (post-data)
  (let* ((key (getf post-data :key))
         (p (getobj key 'product))
         (output (format nil "Product with key ~a not found" key)))
    (when p
      (rename-remove-product-pics p)
      (setf output (format nil "Successfully deleted ~a's pics" key)))
    (soy.admin:pics-deleting (list :output output))))

(defun admin.compile-template (post-data)
  (let ((name (getf post-data :name))
        (output))
    (when post-data
      (setf output
            (if (file-exists-p (pathname (merge-pathnames
                                          (pathname name)
                                          (config.get-option "PATHS" "path-to-templates"))))
                (handler-case
                    (progn
                      (servo.compile-soy name)
                      (format nil "Successfully compiled ~a" name))
                  (error (e) (format  nil "ERROR:~%~a" e)))
                (format nil "File ~a not found" name))))
    (soy.admin:compile-template (list :output output))))

(defun admin.make-backup (post-data)
  (let ((dobackup (getf post-data :dobackup))
        (output))
    (when (string= dobackup "dobackup")
      (setf output
            (handler-case
                (progn
                  (backup.serialize-all)
                  (format nil "Successfully made backup"))
              (error (e) (format  nil "ERROR:~%~a" e)))))
    (soy.admin:make-backup (list :output output))))

(defun admin.do-action (action)
  (handler-case
      (string-case (ensure-string action)
        ("do-gc"
         (sb-ext:gc :full t)
         (htmlize
          (with-output-to-string
              (*standard-output*)
            (room))))
        ("report-products"
         (let ((name (format nil "reports/products-report-~a.csv" (time.encode.backup-filename))))
           (create-report name #'write-products-report)
           "DO PRODUCTS REPORT"))
        ("proccess-pictures"
         (rename-convert-all)
         "DO proccess-pictures")
        ("dtd"
         (dtd)
         "DO DTD")
        ("report"
         (let ((name (format nil "reports/write-groups-active-product-num-~A.csv"
                             (time.encode.backup-filename))))
           (create-report name #'write-groups-active-product-num)
           "DO REPORT"))
        ("articles-restore"
         (articles.restore)
         "RESTORE ARTICLES")
        ("main-page-restore"
         (main-page.restore)
         "RESTORE MAIN-PAGE")
        ("cartrige-restore"
         (cartrige.restore)
         "RESTORE Cartrige")
        ("static-pages-restore"
         (static-pages.restore)
         "STATIC PAGES RESTORE")
        ("groupd-restore"
         (groupd.restore)
         "GROUPD RESTORE")
        ("seo-report"
         (report.do-seo-reports)
         "SEO-REPORT")
        ("keyoptions-aliases-restore"
         (report.do-alias-reports)
         action)
        ("gateway-restore"
         (gateway.restore-history)
         "GATEWAY-RESTORE")
        (t (format nil "DON't know action ~a" action)))
    (error (e) (format  nil "ERROR:~%~a" e))))

(defun admin.parenting-content (post-data)
  (when post-data
    (setf post-data (servo.plist-to-unique post-data))
    (let ((products (ensure-list (getf post-data :products)))
          (groups (ensure-list (getf post-data :groups))))
      (mapcar #'(lambda (product)
                  (mapcar #'(lambda (group)
                              (class-core.bind-product-to-group
                               (getobj product 'product)
                               (getobj group 'group)))
                          groups))
              products)))
  (let ((unparented-products (collect-storage
                              'product
                              :when-fn
                              #'(lambda (item)
                                  (and (null (parent item))
                                       (not (special-p item)))))))
    (soy.class_forms:parenting-page
     (list :products (mapcar #'(lambda (product)
                                 (soy.class_forms:unparented-product-checkbox
                                  (list :key (key product)
                                        :name (name-seo product))))
                             unparented-products)
           :length (length unparented-products)
           :groups (slots.%view 'group-list nil "GROUPS" nil)))))

(defun show-admin-page (&optional (key ""))
  (let ((post-data (servo.alist-to-plist (hunchentoot:post-parameters hunchentoot:*request*))))
    (soy.admin:main
     (list :content
           (string-case (ensure-string key)
             ("info" (soy.admin:info (list :info (admin.get-info))))
             ("actions" (soy.admin:action-buttons (list :post post-data
                                                        :info (admin.do-action
                                                               (getf post-data :action)))))
             ("edit" (admin.edit-content post-data))
             ("make" (admin.make-content post-data))
             ("parenting" (admin.parenting-content post-data))
             ("pics" (admin.pics-deleting post-data))
             ("templates" (admin.compile-template post-data))
             ("backup" (admin.make-backup post-data))
             (t "Админка в разработке"))))))


(defun admin.post-data-preprocessing (post-data)
  (swhen (getf post-data :raw-fullfilter)
    (setf (getf post-data :fullfilter) (decode-fullfilter it)))
  post-data)

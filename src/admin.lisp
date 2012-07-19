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

(restas:define-route admin-test-get-post-request-route ("/administration-super-panel/test-get-post" :method :post)
  (admin.test-get-post-parse))

(restas:define-route admin-test-get-request-route ("/administration-super-panel/test-get-post" :method :get)
  (admin.test-get-post-parse))

(defun admin-compile-templates ()
  (servo.compile-soy "admin.soy"
                     "class_forms.soy"))

(defun admin-update ()
  "Updates templates"
  (admin-compile-templates))

(defun admin.test-get-post-parse ()
  "Parsing get & post parameters for testing"
  (soy.admin:main
   (list :content
         (let* ((get-params (servo.alist-to-plist (hunchentoot:get-parameters hunchentoot:*request*)))
                (post-params (servo.alist-to-plist (hunchentoot:post-parameters hunchentoot:*request*))))
           (format nil "raw GET params: ~a <br />list to unique keys: ~a <br />raw POST params: ~a<br />list to unique keys: ~a"
                   (print get-params) (servo.plist-to-unique get-params)
                   (print post-params) (servo.plist-to-unique post-params))))))


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
    (when (and post-data (string= dobackup "dobackup"))
      (setf output
            (handler-case
                (progn
                  (backup.serialize-all)
                  (format nil "Successfully made backup"))
              (error (e) (format  nil "ERROR:~%~a" e)))))
    (soy.admin:make-backup (list :output output))))

(defun admin.do-action (action)
  (handler-case
      (if (not (null action))
          (string-case action
            ("do-gc"
             (progn
               (sb-ext:gc :full t)
               (regex-replace-all
                "\\n"
                (with-output-to-string
                    (*standard-output*)
                  (room))
                "<br>")))
            ("report-products"
             (progn
               (let ((name (format nil "reports/products-report-~a.csv" (time.encode.backup-filename))))
                 (create-report name #'write-products-report)
                 "DO PRODUCTS REPORT")))
            ("proccess-pictures"
             (progn
               (rename-convert-all)
               "DO proccess-pictures"))
            ("dtd"
             (progn
               (dtd)
               "DO DTD"))
            ("report"
             (progn
               (let ((name (format nil "reports/write-groups-active-product-num-~a.csv" (time.encode.backup-filename))))
                 (create-report name #'write-groups-active-product-num)
                 "DO REPORT")))
            ("articles-restore"
             (articles.restore)
             "RESTORE ARTICLES")
            ("main-page-restore"
             (main-page.restore)
             "RESTORE MAIN-PAGE")
            ("groupd-restore"
             (groupd.restore)
             "RESTORE SALES")
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
            (t (format nil "DON't know action ~a" action))))
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
                                       (not (gethash (key item) *special-products*)))))))
    (soy.class_forms:parenting-page
     (list :products (mapcar #'(lambda (product)
                                 (soy.class_forms:unparented-product-checkbox
                                  (list :key (key product)
                                        :name (name-seo product))))
                             unparented-products)
           :length (length unparented-products)
           :groups (slots.%view 'group-list nil "GROUPS" nil)))))

(defun show-admin-page (&optional (key nil))
  (let ((post-data (servo.alist-to-plist (hunchentoot:post-parameters hunchentoot:*request*))))
    (soy.admin:main
     (list :content
           (cond
             ((null key)
              (format nil "<p> Админка в разработке </p>"))
             ((string= key "info")
              (soy.admin:info (list :info (admin.get-info))))
             ((string= key "actions")
              (soy.admin:action-buttons (list :post post-data
                                              :info (admin.do-action (getf post-data :action)))))
             ((string= key "edit")
              (admin.edit-content post-data))
             ((string= key "make")
              (admin.make-content post-data))
             ((string= key "parenting")
              (admin.parenting-content post-data))
             ((string= key "pics")
              (admin.pics-deleting post-data))
             ((string= key "templates")
              (admin.compile-template post-data))
             ((string= key "backup")
              (admin.make-backup post-data))
             ((string= key "cron-jobs")
              (cron.html-print-job-list post-data))
             (t (format nil "~a" key)))))))


(defun admin.post-data-preprocessing (post-data)
  "keyoptions & aliases (catalog-keyoptions) preprocessing"
  (let ((result post-data)
        (keyoptions)
        (catalog-keyoptions)
        (raw-fullfilter (getf post-data :raw-fullfilter)))
    ;;keyoptions
    (loop
       :for cnt :from 0
       :while (getf post-data
                    (anything-to-keyword (format nil "keyoption-og-~a" cnt)))
       :do (let ((optgroup (getf post-data
                                 (anything-to-keyword (format nil "keyoption-og-~a" cnt))))
                 (optname (getf post-data
                                (anything-to-keyword (format nil "keyoption-on-~a" cnt))))
                 (showname (getf post-data
                                 (anything-to-keyword (format nil "keyoption-sn-~a" cnt))))
                 (units (getf post-data
                              (anything-to-keyword (format nil "keyoption-un-~a" cnt)))))
             (when (and (string/= "" optgroup) (string/= "" optname))
               (push (list :optgroup optgroup :optname optname :showname showname :units units) keyoptions))))
    (setf result (append result (list :keyoptions (nreverse keyoptions))))
    ;;catalog keyoptions
    (loop
       :for cnt :from 0
       :while (getf post-data
                    (anything-to-keyword (format nil "catalog-keyoption-og-~a" cnt)))
       :do (let ((optgroup (getf post-data
                                 (anything-to-keyword (format nil "catalog-keyoption-og-~a" cnt))))
                 (optname (getf post-data
                                (anything-to-keyword (format nil "catalog-keyoption-on-~a" cnt))))
                 (showname (getf post-data
                                 (anything-to-keyword (format nil "catalog-keyoption-sn-~a" cnt))))
                 (units (getf post-data
                              (anything-to-keyword (format nil "catalog-keyoption-un-~a" cnt)))))
             (when (and (string/= "" optgroup) (string/= "" optname) (string/= "" showname))
               (push (list :optgroup optgroup :optname optname :showname showname :units units) catalog-keyoptions))))
    (setf result (append result (list :catalog-keyoptions (nreverse catalog-keyoptions))))
    ;; fullfilter decode
    (if raw-fullfilter
        (let ((new-raw (getf result :raw-fullfilter))
              (new-full))
          (setf new-full (class-core.decode new-raw (make-instance 'group-filter)))
          (setf (getf result :fullfilter) new-full)
          (setf (getf result :raw-fullfilter) new-raw)))
    result))

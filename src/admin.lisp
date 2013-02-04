;;;; admin.lisp

(in-package #:eshop)

(restas:define-route admin-route ("/administration-super-panel")
  (restas:redirect 'admin/-route))

(restas:define-route admin/-route ("/administration-super-panel/")
  (show-admin-page "info"))

(restas:define-route admin-actions-key-route ("/administration-super-panel/actions" :method :post)
  (show-admin-page "actions"))

(restas:define-route admin-pics-route ("/administration-super-panel/pics" :method :post)
  (show-admin-page "pics"))

(restas:define-route admin-templates-route ("/administration-super-panel/templates" :method :post)
  (show-admin-page "templates"))

(restas:define-route admin-backup1-route ("/administration-super-panel/makebackup")
  (show-admin-page "backup"))

(restas:define-route admin-backup-route ("/administration-super-panel/makebackup" :method :post)
  (show-admin-page "backup"))

;; (restas:define-route admin-black-list-route ("/administration-super-panel/black-list")
;;   (show-admin-page "black-list"))

(restas:define-route admin-black-list-post-route ("/administration-super-panel/black-list" :method :post)
  (show-admin-page "black-list"))

(restas:define-route admin-cron-route ("/administration-super-panel/cron-jobs" :method :post)
  (show-admin-page "cron-jobs"))

(restas:define-route admin-parenting-key-route ("/administration-super-panel/parenting" :method :post)
  (show-admin-page "parenting"))

(restas:define-route admin-vendor-seo-route ("/administration-super-panel/vendor-seo" :method :post)
  (show-admin-page "vendor-seo"))

(restas:define-route admin-key-route ("/administration-super-panel/:key")
  (show-admin-page key))

(defun admin.standard-ajax-response (success &optional msg)
  "Standard json-encoded ajax response, include success and msg fields"
  (encode-json-plist-to-string (list :success success :msg (if msg msg "Success"))))

(defun admin.page-wrapper (content)
  "Standard wrapper with styles, scripts, header, etc"
  (soy.admin:main
   (list :content content)))


(restas:define-route admin-filter-create ("administration-super-panel/filter-create" :method :get)
  (string-case (hunchentoot:get-parameter "get")
    ("filter-types"
     ;; FIXME: use json encode, not format
     (format nil "[~{~A~^,~}]" (mapcar #'encode-json-plist-to-string
                                       (filters.get-basics-types))))
    ("fields"
     (format nil "[~{~A~^,~}]" (mapcar #'encode-json-plist-to-string
                                       (filters.get-basic-fields (hunchentoot:get-parameter "filter-type")))))))

(restas:define-route admin-edit-slot-route ("administration-super-panel/edit-slot" :method :post)
  (let ((object (getobj (hunchentoot:post-parameter "key")))
        (slot (anything-to-symbol (hunchentoot:post-parameter "slot")))
        (value (hunchentoot:post-parameter "value")))
    (if object
        (handler-case
            (progn
              (setf (slot-value object slot)
                    (slots.%get-data (slot-type (type-of object) slot) value))
              ;; TODO: write slot-specific fixers (which return whether whole-object fix is needed)
              ;; FIXME: bad code
              (when (and (groupp object) (equal slot 'raw-fullfilter))
                (setf (fullfilter object) (decode-fullfilter (raw-fullfilter object))))
              (when (equal slot 'parents)
                (typecase object
                  (product
                   (class-core.unbind-product-from-all-groups object)
                   (mapcar #'(lambda (group) (class-core.bind-product-to-group object group)) (parents object)))
                  (group
                   (class-core.unbind-group-from-all-parents object)
                   (mapcar #'(lambda (parent) (class-core.bind-group-to-parent object parent)) (parents object)))))
              ;; return value
              (admin.standard-ajax-response t))
          (error (e) (admin.standard-ajax-response nil (format nil "Error: ~A" e))))
        ;; else
        (admin.standard-ajax-response nil "Error: Object doesn't exist"))))


(defun admin-compile-templates ()
  (servo.compile-soy "admin.soy"
                     "class_forms.soy"))

(defun admin-update ()
  "Updates templates"
  (admin-compile-templates))

(defun admin.get-info ()
  ;; (setf (bt:thread-name (bt:current-thread)) "test")
  (list (format nil "~A<br>~{~{~@[~A~]~^: ~}<br>~}"
                (concatenate 'string "<b>Последняя выгрузка: "
                                     (time.encode.backup (date *gateway.loaded-dump*))
                                     " || продуктов: "
                                     (write-to-string (product-num *gateway.loaded-dump*))
                                     " || активных: "
                                     (write-to-string (count-storage 'product :when-fn #'active))
                                     " || выключенных: "
                                     (write-to-string (hash-table-count black-list.*storage*))
                                     "</b>")
                (mapcar #'(lambda (v) (let ((thread-name (bt:thread-name v)))
                                        (list thread-name)))
                                  (sb-thread:list-all-threads)))
        (regex-replace-all "\\n" (with-output-to-string (*standard-output*) (room)) "<br>")))


(restas:define-route admin-make-get-route ("/administration-super-panel/make" :method :get)
  (admin.page-wrapper
   (let ((key (hunchentoot:get-parameter "key"))
         (type (hunchentoot:get-parameter "type")))
     (if (getobj key)
         (restas:redirect 'admin-edit-get-route)
         ;; else
         (if (and (valid-string-p type)
                  (class-exist-p (anything-to-symbol type)))
             (soy.class_forms:formwindow
              (list :key key
                    :type type
                    :fields (class-core.make-fields (get-instance type))
                    :target "make"))
             ;; else
             "Incorrect type or no type specified")))))

(restas:define-route admin-make-choose-key-route ("/administration-super-panel/make-key" :method :get)
  (admin.page-wrapper
   (soy.class_forms:make-choose-key)))

(restas:define-route admin-new-make-post-route ("/administration-super-panel/new-make" :method :post)
  (debug-slime-format "~A " (hunchentoot:post-parameters*))
  (let ((type (string-downcase (tbnl:post-parameter "type")))
        (key (tbnl:post-parameter "key")))
    (if (getobj key)
        (restas:redirect 'admin-edit-get-route :type type :key key)
        (progn
          ;; FIXME
          (setobj key (make-instance (string-case (string-downcase type)
                                       ("product" 'product)
                                       ("group" 'group)) :key key))
          (admin.post-make-fix (getobj key))
          (restas:redirect 'admin-edit-get-route :type type :key)))))

(defgeneric admin.post-make-fix (item)
  (:documentation "Perform class-specific fixes after creating instance")
  (:method (item) #| do nothing by default |#))

(defmethod admin.post-make-fix ((item product))
  (setf
   (articul item) (parse-integer (key item))
   (date-created item) (get-universal-time)
   (date-modified item) (get-universal-time))
   ;; make pointers to this product from parents
   (mapcar #'(lambda (parent)
               (push item (products parent)))
           (parents item)))

(defmethod admin.post-make-fix ((item group))
  ;; upsale
  (setf (empty item) (notany #'active (products item)))
  (mapcar #'(lambda (parent)
              (push item (groups parent)))
          (parents item))
  (when (raw-fullfilter item)
    (setf (fullfilter item) (decode-fullfilter (raw-fullfilter item)))))

(defmethod admin.post-make-fix ((item vendor))
  (let ((name (name item)))
    (when (valid-string-p name)
      (setobj (string-downcase name) item 'vendor))))

(defmethod admin.post-make-fix ((item filter))
  (mapcar #'(lambda (parent)
              (setf (gethash (key item) (filters parent)) item))
          (parents item)))

(restas:define-route admin-make-post-route ("/administration-super-panel/make" :method :post)
  (let* ((key (hunchentoot:post-parameter "key"))
         (type (anything-to-symbol (hunchentoot:post-parameter "type")))
         (item (getobj key)))
    (debug-slime-format "~A" (hunchentoot:post-parameters*))
    (if (not (class-exist-p type))
        ;; return error
        (admin.standard-ajax-response nil "Unknown type")
        ;; else
        (progn
          (unless item ; should always be true
            (log5:log-for info "Create new item from admin panel with key: ~A" key)
            (setf item (make-instance-from-post-data type))
            (admin.post-make-fix item)
            (setobj key item)) ; adding item into storage
          ;; return success
          (admin.standard-ajax-response t)))))

(restas:define-route admin-edit-get-route ("/administration-super-panel/edit" :method :get)
  ;; TODO: use type parameter
  (admin.page-wrapper
   (let* ((key (hunchentoot:get-parameter "key"))
          (item (getobj key)))
     (if item
         (soy.class_forms:formwindow
          (list :key key
                :fields (class-core.make-fields item)
                :target "edit"))
         ;; else
         "Item with specified key is not found"))))

;; TODO: rewrite using slot-specific fixers (dont forget about post-make-fix and post-unserialize)
(defgeneric admin.post-edit-fix (item)
  (:documentation "Perform class-specific fixes after changing instance slots ")
  (:method (item) #| do nothing by default |#))

(defmethod admin.post-edit-fix ((item product))
  (setf (date-modified item) (get-universal-time)))

(defmethod admin.post-edit-fix ((item group))
  (when (raw-fullfilter item)
    (setf (fullfilter item) (decode-fullfilter (raw-fullfilter item)))))

(restas:define-route admin-edit-post-route ("/administration-super-panel/edit" :method :post)
  (let* ((key (hunchentoot:post-parameter "key"))
         (item (getobj key)))
    (if item
        (handler-case
            (progn
              (class-core.edit-slots item)
              (admin.post-edit-fix item)
              (slots.parents-fix item)
              (admin.standard-ajax-response t))
          (error (e) (admin.standard-ajax-response nil (format nil "ERROR: ~A" e))))
        ;; else
        (admin.standard-ajax-response nil "Item with specified key is
        not found"))))


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
                                          (config.get-option :paths :path-to-templates))))
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
         (create-report (format nil "products-report-~A.csv" (time.encode.backup-filename))
                        #'report.product-report)
         "DO PRODUCTS REPORT")
        ("proccess-pictures"
         (rename-convert-all)
         "DO proccess-pictures")
        ("dtd"
         (xls.update-options-from-xls)
         "DO DTD")
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
        ("groups-products-report"
         (report.do-groups-products-report)
         "groups-products-report")
        ("keyoptions-aliases-restore"
         (report.do-alias-reports)
         action)
        ("gateway-restore"
         (gateway.load)
         "GATEWAY-RESTORE")
        (t (format nil "DON't know action ~A<br>~A" action (admin.get-info))))
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

(defun admin.vendor-seo-upload (post-data)
  (let* ((get-params (servo.alist-to-plist (hunchentoot:get-parameters hunchentoot:*request*)))
         (group-key (getf post-data :group))
         (vendor-key (getf post-data :vendor))
         (new-text (getf post-data :text)))
    (debug-slime-format "~A ~A" post-data get-params)
    (cond
      ((and new-text (not (and group-key vendor-key))) "Error: please specify vendor and group")
      ((and new-text (not (getobj group-key 'group))) "Error: group not found")
      ((and new-text (not (getobj vendor-key 'vendor))) "Error: vendor not found")
      (t (when new-text
           (setf (gethash group-key (seo-texts (getobj vendor-key 'vendor))) new-text))
         (soy.admin:vendor-seo-upload (list :text (awhen (getobj vendor-key 'vendor)
                                                    (gethash group-key (seo-texts it)))))))))


(defun admin.black-list (post-data)
  (let* ((key (getf post-data :key))
         (output nil)
         (errortext nil))
    (when key
      (aif (getobj key)
           (progn
             (black-list.insert it)
             (black-list.%deactive it)
             (setf output (format nil "Товар <a href='/~A'>~A</a>:~A убран"
                                  (key it) (key it) (name-seo it))))
           (setf errortext "Нет такого товара")))
    (soy.admin:black-list (list :output output :errortext errortext))))

(defun show-admin-page (&optional (key ""))
  (let ((post-data (servo.alist-to-plist (hunchentoot:post-parameters hunchentoot:*request*))))
    (soy.admin:main
     (list :content
           (string-case (ensure-string key)
             ("info" (soy.admin:info (list :info (admin.get-info))))
             ("actions" (soy.admin:action-buttons (list :post post-data
                                                        :info (admin.do-action
                                                               (getf post-data :action)))))
             ("parenting" (admin.parenting-content post-data))
             ("pics" (admin.pics-deleting post-data))
             ("templates" (admin.compile-template post-data))
             ("backup" (admin.make-backup post-data))
             ("black-list" (admin.black-list post-data))
             ("vendor-seo" (admin.vendor-seo-upload post-data))
             (t "Админка в разработке"))))))

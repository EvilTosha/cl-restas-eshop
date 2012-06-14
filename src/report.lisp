;;;; report.lisp

(in-package #:eshop)

(defparameter *special-products* (make-hash-table :test #'equal))

(defun write-products-report (stream)
  (format stream "~a;~a;~a;~a;~a;~a;~a;~a;~a;~a;~a;~a;~a;~a;~a;~a;~a;~%"
          "артикул" "цена магазина" "цена сайта" "имя" "имя real" "имя yml" "is-yml-show" "seo текст"
          "фотографии" "характеристики" "активный" "группа" "родительская группа"
          "secret" "DTD" "vendor" "доставка")
  (process-storage
   #'(lambda (v)
       (let ((id "нет") (name "нет") (name-real "нет") (name-yml "нет")
             (desc "нет") (img "нет") (options "нет") (active "нет")
             (group-name "нет") (parent-group-name "нет") (secret "нет"))
         (setf id (articul v))
         (setf name (stripper (name-provider v)))
         (setf name-real (stripper (name-seo v)))
         (with-option1 v "Secret" "Yandex"
                       (setf name-yml (stripper (getf option :value))))
         (setf desc (if (and (not (null (seo-text v)))
                             (not (string= "" (stripper (seo-text v)))))
                        "есть"
                        "нет"))
         (setf img (length (get-pics (articul v))))
         (setf options (if (is-valide-option v)
                           "есть"
                           "нет"))
         (setf active (if (active v)
                          "да"
                          "нет"))
         (setf group-name (when (class-core.parent v)
                            (stripper (name (class-core.parent v)))))
         (setf parent-group-name (when (and (class-core.parent v)
                                            (class-core.parent (class-core.parent v)))
                                   (stripper (name (class-core.parent (class-core.parent v))))))
         (setf secret "Нет")
         (with-option1 v "Secret" "Checked"
                       (setf secret (getf option :value)))
         (format stream "~a;~a;~a;\"~a\";\"~a\";\"~a\";~a;~a;~a;~a;~a;\"~a\";\"~a\";~a;~a;~a;~a~%"
                 id (price v) (siteprice v) name name-real
                 name-yml (yml.yml-show-p v) desc img options active group-name
                 parent-group-name secret
                 (gethash (articul v) *xls.product-table*)
                 (vendor v)
                 (yml.get-product-delivery-price1 v))))
   'product))

(defun is-valide-option (product)
  (let ((flag nil))
    (mapcar #'(lambda (v) (mapcar #'(lambda (l)
                                      (when (and (not (equal (getf v :name) "Secret"))
                                                 l
                                                 (getf l :value)
                                                 (not (equal
                                                       (string-trim (list #\Space #\Tab #\Newline) (getf l :value))
                                                       ""))
                                                 (not (equal (getf l :name) "Производитель"))
                                                 (not (equal (getf l :name) "Модель")))
                                        (setf flag t)))
                                  (getf v :options)))
            (optgroups product))
    flag))

(defun write-groups (stream)
  (format stream "~a;~a;~a;~a;~a;~a;~%"
          "Название категории"
          "url страницы"
          "Active"
          "seo-text"
          "продуктов"
          "активных")
  (process-storage
   #'(lambda (v)
       (format stream "\"~a\";http://www.320-8080.ru/~a;~a;~a;~a;~a;~%"
               (stripper (name v))
               (key v)
               (if (active v)
                   "yes"
                   "no")
               (if (servo.valid-string-p (seo-text v))
                   "yes"
                   "no")
               (length (products v))
               (length (remove-if-not #'active (products v)))))
   'group))

(defun write-groups-active-product-num (stream)
  (format stream "~a;~a;~a;~a;~%"
          "Название категории"
          "url страницы"
          "Active"
          "кол-во товаров")
  (process-storage
   #'(lambda (v)
       (format stream "\"~a\";http://www.320-8080.ru/~a;~a;~a;~%"
               (stripper (name v))
               (key v)
               (if (active v)
                   "yes"
                   "no")
               (length (storage.get-recursive-products v #'active))))
   'group))



(defun write-products (stream)
  (let ((vendor-name)
        (desc))
    (format stream "~a;~a;~a;~a;~a;~a;~%"
            "Название категории"
            "Брэнд"
            "Название товара"
            "url страницы"
            "Active"
            "seo-text")
    (process-storage
     #'(lambda (v)
         (setf vendor-name "Нет")
         (setf vendor-name (vendor v))
         (setf desc (if (servo.valid-string-p (seo-text v))
                        "yes"
                        "no"))
         (format stream "\"~a\";\"~a\";\"~a\";http://www.320-8080.ru/~a;~a;~a;~%"
                 (if (class-core.parent v)
                     (stripper (name (class-core.parent v)))
                     "Нет категории")
                 (stripper vendor-name)
                 (stripper (name-seo v))
                 (articul v)
                 (if (active v)
                     "yes"
                     "no")
                 desc))
    'product)))


(defun write-vendors (stream)
  (format stream "~a;~a;~a;~a;~a;~a;~a;~%"
          "Название категории"
          "Брэнд"
          "url страницы"
          "Active"
          "seo-text"
          "продуктов"
          "активных")
  (process-storage
   #'(lambda (v)
       (unless (groups v)
         (maphash #'(lambda (vendor num)
                      (declare (ignore num))
                      (let ((products
                             (remove-if-not #'(lambda (p)
                                                (vendor-filter-controller p vendor))
                                            (products v))))
                        (format stream "\"~a\";\"~a\";http://www.320-8080.ru/~a?vendor=~a;~a;~a;~a;~a;~%"
                                (stripper (name v))
                                (stripper vendor)
                                (hunchentoot:url-encode (key v))
                                (hunchentoot:url-encode (stripper vendor))
                                "yes"
                                (if (class-core.has-vendor-seo-text v vendor)
                                    "yes"
                                    "no")
                                (length products)
                                (length (remove-if-not #'active products)))))
                  (storage.get-vendors (storage.get-filtered-products v #'atom)))))
   'group))

(defun create-report (file-name report-func)
  (let ((filename (format nil "~a/~a" *path-to-dropbox* file-name)))
    (with-open-file
        (stream filename :direction :output :if-exists :supersede :external-format :cp1251)
      (print stream)
      (funcall report-func stream))))


(defun post-proccess-gateway ()
  (mapcar #'(lambda (v)
              (let ((p (getobj v 'product)))
                (when p
                  (setf (preorder p) t)
                  (setf (active p) t)
                  (setobj v p)
                  (setf (gethash v *special-products*) p))))
          (list
           "999888"
           "711265"
           "834786"
           "938111"
           "777888"
           "888777"
           "999111"
           "999777"))
  (let ((rs))
    (process-storage
     #'(lambda (v)
         (when (and (active v)
                    (zerop (siteprice v)))
           (push v rs)
           (setf (active v) nil)))
     'product)
    (length rs)))

(defun product-delivery (p)
  (let ((g (parent p))
        (daily (gethash (articul p) (daily *main-page.storage*))))
    (if daily
        0
        (aif (delivery-price p)
             it
             (if (and g (delivery-price g))
                 (delivery-price g)
                 300)))))

(defun report.delete-doubles (products)
  (mapcar #'(lambda (v)
              (remobj (format nil "~A" v) 'product))
          products))

(defun report.add-products-to-group (product-list gr)
  (mapcar #'(lambda (v)
              (let ((pr (getobj (format nil "~a" v) 'product)))
                (when pr
                  (setf (parents pr) (list gr))
                  (push pr (products gr)))))
          product-list)
  "done")

(defun edit-marketing-filter (group key-suffix name func)
  (let* ((key (format nil "~A-~A" (key group) key-suffix))
         new-filter
         (filter (aif (find key (filters group) :test #'equal :key #'key)
                      it
                      (progn
                        (setf new-filter t)
                        (setobj key (make-instance 'filter))))))
    (setf (name filter) name)
    (setf (func filter) func)
    (setf (key filter) key)
    (setf (parents filter) (list group))
    (when new-filter
      (push filter (filters group)))
    filter))

(defun create-sale-filter (group)
  (edit-marketing-filter
   group "sale" "Распродажа" #'groupd.is-groupd))

(defun create-bestprice-filter (group)
  (edit-marketing-filter
   group "bestprice" "Горячий уик-энд скидок"
   #'(lambda (object) (plusp (delta-price object)))))

(defun create-ipad3-filter (group)
  (edit-marketing-filter
   group "ipad3" "IPad 3"
   #'(lambda (p)
       (with-option1 p "Общие характеристики" "Модель"
                     (awhen (getf option :value)
                       (string= (format nil "~(~A~)" it) "ipad new"))))))

(defun create-man-sale-filter (group)
  (edit-marketing-filter
   group "23feb" "Подарки к 23 февраля" #'groupd.man.is-groupd))

(defun create-woman-sale-filter (group)
  (edit-marketing-filter
   group "8mart" "Подарки к 8 марта" #'groupd.woman.is-groupd))

(defun report.set-man-salefilter ()
  (process-storage #'create-man-sale-filter 'group))

(defun report.set-woman-salefilter ()
  (process-storage #'create-woman-sale-filter 'group))


(defun report.set-filters (groups filter-func name filter-key)
  (mapcar #'(lambda (gr)
              (edit-marketing-filter gr filter-key name filter-func))
          groups))

(defun report.create-marketing-filters ()
	(create-ipad3-filter (getobj "planshetnie-komputery" 'group))
	(report.set-filters (list (getobj "noutbuki" 'group))
											#'(lambda (product)
													(let ((opts))
														(with-option1 product
															"Общие характеристики" "Тип устройства"
															(setf opts (getf option :value)))
														(equal opts "Ультрабук")))
											"Ультрабуки"
											"ultrabooks")
  ;; TODO: убрать костыль
	(report.set-filters (process-and-collect-storage 'group)
											#'groupd.holiday.is-groupd
											"Для отдыха"
											"holidays"))

(defun report.set-salefilter ()
  (mapcar #'(lambda (v)
              (create-sale-filter (getobj (format nil "~a" v) 'group)))
          (list "netbuki"
                "noutbuki"
                "planshetnie-komputery"
                "cifrovye-fotoapparaty"
                "lcd-televizory"
                "monitory"
                "printery"
                "mfu"
                "myshki"
                "klaviatury")))

(defun report.convert-name (input-string)
  (string-trim (list #\Space #\Tab #\Newline)
               (format nil "~:(~a~)" input-string)))

(defun report.get-groups (group)
  (if (null (groups group))
      (format t "~&(equal key \"~a\")" (key group))
      (mapcar #'report.get-groups (groups group))))


(defun report.do-seo-reports ()
  (let ((name (format nil "reports/seo-report-groups-~a.csv" (time.encode.backup-filename))))
    (log5:log-for info "Do groups SEO report")
    (create-report name #'write-groups))
  (let ((name (format nil "reports/seo-report-vendors-~a.csv" (time.encode.backup-filename))))
    (log5:log-for info "Do vendors SEO report")
    (create-report name #'write-vendors))
  (let ((name (format nil "reports/seo-report-products-~a.csv" (time.encode.backup-filename))))
    (log5:log-for info "Do products SEO report")
    (create-report name #'write-products)))


(defun report.write-alias (&optional (stream *standard-output*))
  (format stream "имя группы; наличие алиасов; группа опций ; опция; имя алиаса; ед. измерения;")
  (process-and-collect-storage
   'group
   :func #'(lambda (gr)
             (if (null (catalog-keyoptions gr))
                 (format stream "~&~a; нет;" (name gr))
                 (mapcar #'(lambda (alias)
                             (let ((alias-temp (mapcar #'stripper
                                                       (remove-if #'keywordp alias))))
                               (format stream "~&\"~a\";~a;~{\" ~a \";~}"
                                       (stripper (name gr))
                                       (if alias-temp "есть" "нет")
                                       alias-temp)))
                         (catalog-keyoptions gr))))))

(defun report.write-keyoptions (&optional (stream *standard-output*))
  (format stream "имя группы; наличие ключевых опций; группа опций ; опция;")
  (process-and-collect-storage
   'group
   :func #'(lambda (gr)
             (if (null (keyoptions gr))
                 (format stream "~&~a; нет;" (name gr))
                 (mapcar #'(lambda (alias)
                             (let ((alias-temp (mapcar #'stripper
                                                       (remove-if #'keywordp alias))))
                               (format stream "~&\"~a\";~a;~{\" ~a \";~}"
                                       (stripper (name gr))
                                       (if alias-temp "есть" "нет")
                                       alias-temp)))
                         (keyoptions gr))))))


(defun report.do-alias-reports ()
  (progn
    (let ((name (format nil "reports/aliases-report-~a.csv" (time.encode.backup-filename))))
      (create-report name #'report.write-alias)
      "AliAS REPORT DONE"))
  (progn
    (let ((name (format nil "reports/keyoptions-report-~a.csv" (time.encode.backup-filename))))
      (create-report name #'report.write-keyoptions)
      "KEYOPTIONS REPORT DONE")))


(defun report.check-product-picture (product &optional (stream *standard-output*))
  (let* ((articul (articul product))
         (is-need-reconvert nil)
         (path-art  (ppcre:regex-replace  "(\\d{1,3})(\\d{0,})"  (format nil "~a" articul)  "\\1/\\1\\2" )))
    (mapcar #'(lambda (pic)
                (let ((src-pic-path
                       (format nil "~a/~a/~a/~a"
                               (config.get-option "PATHS" "path-to-pics") "big" path-art pic)))
                  (with-open-file
                      (stream-file src-pic-path)
                    (when (zerop (file-length stream-file))
                      (setf is-need-reconvert t)
                      (format stream "~&~a;~a" articul (file-length stream-file))))))
            (get-pics articul))
    is-need-reconvert))


(defun report.write-pictures (&optional (stream *standard-output*))
  (let ((num 0))
    (format stream "артикул;имя;файл;ширина;высота;размер;")
    (process-storage
     #'(lambda (gr)
         (mapcar #'(lambda (product)
                     (let* ((articul (articul product))
                            (path-art  (ppcre:regex-replace  "(\\d{1,3})(\\d{0,})"  (format nil "~a" articul) "\\1/\\1\\2")))
                       (mapcar #'(lambda (pic)
                                   (let ((src-pic-path
                                          (format nil "~a/~a/~a/~a"
                                                  (config.get-option "PATHS" "path-to-pics") "big" path-art pic)))
                                     (with-open-file
                                         (stream-file src-pic-path)

                                       (when (zerop (file-length stream-file))
                                         (incf num)
                                         (format stream "~&~a;~a" articul (file-length stream-file))))))
                               (get-pics articul))))
                 (products gr)))
     'group)
    num))

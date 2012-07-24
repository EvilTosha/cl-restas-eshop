;;;; report.lisp

(in-package #:eshop)

(defparameter *special-products* (make-hash-table :test #'equal))

(defgeneric special-p (product)
  (:documentation "Checks whether product is in *special-products*"))

(defmethod special-p ((product string))
  (not (null (gethash product *special-products*))))

(defmethod special-p ((product product))
  (when product
    (special-p (key product))))

(defun write-products-report (stream)
  (labels ((get-2-lvl-group (obj)
             (when (parent obj)
               (if (parent (parent obj))
                   (get-2-lvl-group (parent obj))
                   obj))))
    (format stream "~a;~a;~a;~a;~a;~a;~a;~a;~a;~a;~a;~a;~a;~a;~a;~a;~a;~a;~a;~%"
            "артикул" "цена магазина" "цена сайта" "имя" "имя real" "имя yml" "is-yml-show" "seo текст"
            "фотографии" "характеристики" "активный" "группа" "родительская группа" "группа 2-го уровня"
            "secret" "DTD" "vendor" "доставка" "серия")
    (process-storage
     #'(lambda (v)
         (let ((id "нет") (name "нет") (name-real "нет") (name-yml "нет")
               (desc "нет") (img "нет") (options "нет") (active "нет")
               (group-name "нет") (parent-group-name "нет") (2-group-name "нет") (secret "нет") (seria "нет"))
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
           (setf options (if (valid-option-p v)
                             "есть"
                             "нет"))
           (setf active (if (active v)
                            "да"
                            "нет"))
           (setf group-name (when (parent v)
                              (stripper (name (parent v)))))
           (setf parent-group-name (when (and (parent v)
                                              (parent (parent v)))
                                     (stripper (name (parent (parent v))))))
           (setf 2-group-name (when (and (parent v) (parent (parent v)))
                                (stripper (name (get-2-lvl-group v)))))
           (setf secret "Нет")
           (with-option1 v "Secret" "Checked"
                        (setf secret (getf option :value)))
           (with-option1 v "Общие характеристики" "Серия"
                        (setf seria (getf option :value)))
           (format stream "~a;~a;~a;\"~a\";\"~a\";\"~a\";~a;~a;~a;~a;~a;\"~a\";\"~a\";\"~a\";~a;~a;~a;~a;\"~a\"~%"
                   id (price v) (siteprice v) name name-real
                   name-yml (yml.yml-show-p v) desc img options active group-name
                   parent-group-name 2-group-name secret
                   (gethash (articul v) *xls.product-table*)
                   (vendor v)
                   (yml.get-product-delivery-price1 v)
                   seria)))
     'product)))

(defun valid-option-p (product)
  (declare (product product))
  (some #'(lambda (optgroup) (some #'(lambda (option)
                                       (and option
                                            (valid-string-p (getf option :value))
                                            (not (find (getf option :name)
                                                       (list "Производитель" "Модель") :test #'equal))))
                                   (getf optgroup :options)))
        (remove "Secret" (optgroups product)  ; remove Secret group
                :test #'equal :key #'(lambda (opt) (getf opt :name)))))

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
               (if (valid-string-p (seo-text v))
                   "yes"
                   "no")
               (length (products v))
               (count-if #'active (products v))))
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
         (setf desc (if (valid-string-p (seo-text v))
                        "yes"
                        "no"))
         (format stream "\"~a\";\"~a\";\"~a\";http://www.320-8080.ru/~a;~a;~a;~%"
                 (if (parent v)
                     (stripper (name (parent v)))
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
                                (count-if #'active products))))
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
           "666616"
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

(defun report.convert-name (input-string)
  (string-trim (list #\Space #\Tab #\Newline)
               (format nil "~:(~a~)" input-string)))

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
  (collect-storage
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
  (collect-storage
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

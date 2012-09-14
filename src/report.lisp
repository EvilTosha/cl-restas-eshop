;;;; report.lisp

;; TODO: move to new package report
(in-package #:eshop)

(defparameter *special-products* (make-hash-table :test #'equal))

(defgeneric special-p (product)
  (:documentation "Checks whether product is in *special-products*"))

(defmethod special-p ((product string))
  (not (null (gethash product *special-products*))))

(defmethod special-p ((product product))
  (when product
    (special-p (key product))))

;;;; --------------------------- report mechanism -------------------------------
(defvar report.*standard-report-column-funcs* (make-hash-table :test #'equal)
  "Hash-table for storing standard functions for creating reports by ip.
Key should be (unique) symbol, value - function")

(defun report.register-standard-column (specifier func)
  "Registers function that lately can be used in reports' creating API.
Specifier should be symbol, func should be function.
Result of each function must be formatable (e.g. (format nil \"~A\") must be applicable to it)"
  (declare (symbol specifier) (function func))
  (setf (gethash specifier report.*standard-report-column-funcs*) func))

(defun report.get-standard-column-func (specifier)
  (gethash specifier report.*standard-report-column-funcs*))

(defun report.write-report (stream column-headers column-funcs items)
  "Writes report in .csv format to given stream. Each row is set of column functions
applied to item from given item set."
  (declare (stream stream) (list column-headers column-funcs items))
  ;; write headers
  (format stream "~{~A;~}~%" column-headers)
  ;; write other rows
  (mapcar #'(lambda (item)
              (format stream "~{~A;~}~%"
                      (mapcar #'(lambda (func)
                                  (funcall func item))
                              column-funcs)))
          items))

(defun report.write-report-with-standard-columns (stream columns-data storage-specifier)
  "Writes report using only registered columns functions. Column data should be
list of conses (column-header . column-specifier). Storage-specifier should be symbol, to which
function get-storage is applicable"
  (declare (stream stream) (list columns-data) (symbol storage-specifier))
  (loop
     :for (header . specifier) :in columns-data
     :collect header :into headers
     :collect (report.get-standard-column-func specifier) :into funcs
     :finally (report.write-report stream headers funcs (collect-storage storage-specifier))))


;;;; register standard columns
;;; product functions
(report.register-standard-column 'product-articul #'articul)
(report.register-standard-column 'product-price #'price)
(report.register-standard-column 'product-siteprice #'siteprice)
(report.register-standard-column 'product-name #'name-provider)
(report.register-standard-column 'product-name-real #'name-seo)
(report.register-standard-column
 'product-yml-name
 #'(lambda (item) (get-option item "Secret" "Yandex")))
(report.register-standard-column 'product-yml-show #'yml.yml-show-p)
(report.register-standard-column
 'product-seo-text-exist
 #'(lambda (item) (if (valid-string-p (seo-text item)) "есть" "нет")))
(report.register-standard-column
 'product-num-pics
 #'(lambda (item) (length (get-pics (articul item)))))
(report.register-standard-column 'product-valid-options #'valid-options)
(report.register-standard-column
 'product-active
 #'(lambda (item) (if (active item) "да" "нет")))
(report.register-standard-column
 'product-group
 #'(lambda (item) (when (parent item) (name (parent item)))))
(report.register-standard-column
 'product-grandparent
 #'(lambda (item) (when (and (parent item) (parent (parent item)))
                   (name (parent (parent item))))))
;; return name of 2 level group(counting from root, root group has 1 level),
;; which is ancestor of given item
(report.register-standard-column
 'product-2-lvl-group
 #'(lambda (item) (loop
                     :for cur := (parent item) :then (parent cur)
                     :while (and cur (parent cur) (parent (parent cur)))
                     :finally (return (when (and cur (parent cur)) (name cur))))))
(report.register-standard-column
 'product-secret #'(lambda (item) (get-option item "Secret" "Checked")))
(report.register-standard-column
 'product-dtd
 #'(lambda (item)
            (gethash (articul item) *xls.product-table*)))
(report.register-standard-column 'product-vendor #'vendor)
(report.register-standard-column 'product-delivery #'yml.get-product-delivery-price1)
(report.register-standard-column
 'product-seria #'(lambda (item) (get-option item "Общие характеристики" "Серия")))
(report.register-standard-column
 'product-direct-name #'(lambda (item) (get-option item "Secret" "Direct-name")))
(report.register-standard-column
 'product-double #'(lambda (item) (get-option item "Secret" "Дубль")))
(report.register-standard-column
 'product-warranty #'(lambda (item) (get-option item "Дополнительная информация" "Гарантия")))

;;; group functions
(report.register-standard-column
 'group-name #'name)
(report.register-standard-column
 'group-url
 #'(lambda (item)
     ;; TODO: use restas url designator
     (format nil "http://www.320-8080.ru/~A" (key item))))
(report.register-standard-column
 'group-active #'(lambda (item) (if (active item) "yes" "no")))
(report.register-standard-column
 'group-seo-text
 #'(lambda (item)
     (if (valid-string-p (seo-text item)
                         "yes" "no"))))
(report.register-standard-column
 'group-count-products
 #'(lambda (item)
     (length (products item))))
(report.register-standard-column
 'group-count-active-products
 #'(lambda (item)
     (count-if #'active (products item))))

(defun report.product-report (stream)
  (report.write-report-with-standard-columns
   stream
   (list (cons "артикул" 'product-articul)
         (cons "цена магазина" 'product-price)
         (cons "цена сайта" 'product-siteprice)
         (cons "имя" 'product-name)
         (cons "имя real" 'product-name-real)
         (cons "имя yml" 'product-yml-name)
         (cons "is-yml-show" 'product-yml-show)
         (cons "seo текст" 'product-seo-text-exist)
         (cons "фотографии" 'product-num-pics)
         (cons "характеристики" 'product-valid-options)
         (cons "активный" 'product-active)
         (cons "группа" 'product-group)
         (cons "родительская группа" 'product-grandparent)
         (cons "группа 2-го уровня" 'product-2-lvl-group)
         (cons "secret" 'product-secret)
         (cons "DTD" 'product-dtd)
         (cons "vendor" 'product-vendor)
         (cons "доставка" 'product-delivery)
         (cons "серия" 'product-seria)
         (cons "direct-name" 'product-direct-name)
         (cons "дубль" 'product-double)
         (cons "гарантия" 'product-warranty))
   'product))

(defun report.group-report (stream)
  (report.write-report-with-standard-columns
   stream
   (list (cons "Название категории" 'group-name)
         (cons "url страницы" 'group-url)
         (cons "Active" 'group-active)
         (cons "seo-text" 'group-seo-text)
         (cons "продуктов" 'group-count-products)
         (cons "активных" 'group-count-active-products))))

(defun valid-options (product)
  (declare (product product))
  (let ((num 0))
    (mapcar #'(lambda (optgroup)
                (incf num
                      (count-if #'(lambda (option)
                                    (and option
                                         (valid-string-p (getf option :value))
                                         (not (find (getf option :name)
                                                    (list "Производитель" "Модель") :test #'equal))))
                                (getf optgroup :options))))
            (remove "Secret" (optgroups product)  ; remove Secret group
                    :test #'equal :key #'(lambda (opt) (getf opt :name))))
    num))

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
    (create-report name #'report.group-report))
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

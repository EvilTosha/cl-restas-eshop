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

(defun valid-options (product)
  (declare (product product))
  (let ((num 0))
    (mapcar #'(lambda (optgroup)
                (incf num
                      (count-if #'(lambda (option)
                                    (and option
                                         (valid-string-p (getf option :value))
                                         (not (find (getf option :name)
                                                    (list "Производитель" "Модель" "Гарантия" "Сайт производителя") :test #'equal))))
                                (getf optgroup :options))))
            (remove "Secret" (optgroups product)  ; remove Secret group
                    :test #'equal :key #'(lambda (opt) (getf opt :name))))
    num))

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

(defun report.write-item (stream column-funcs item)
  "Writes single report row to stream"
  (format stream "~{\"~A\";~}~%"
          (mapcar #'(lambda (func)
                      (servo.string-replace-chars
                       (format nil "~a" (funcall func item))
                       '(#\" #\;)))
                  column-funcs)))

(defun report.write-header (stream column-headers)
  "Write headers for columns to header"
  (declare (list column-headers))
  (format stream "~{\"~A\";~}~%" column-headers))

(defun report.write-report (stream column-headers column-funcs items)
  "Writes report in .csv format to given stream. Each row is set of column functions
applied to item from given item set.
   NOTE: - stream could be T
         - remove all #\" & #\; for compatibility csv"
  (declare (list column-headers column-funcs items))
  ;; write headers
  (report.write-header stream column-headers)
  ;; write other rows
  (mapcar (alexandria:curry #'report.write-item stream column-funcs) items))

(defun report.write-report-with-standard-columns (stream columns-data storage-specifier)
  "Writes report using only registered columns functions. Column data should be
list of conses (column-header . column-specifier). Storage-specifier should be symbol, to which
function get-storage is applicable.
 NOTE: stream could be T & NIL"
  (declare (list columns-data) (symbol storage-specifier))
  (loop
     :for (header . specifier) :in columns-data
     :collect header :into headers
     :collect (report.get-standard-column-func specifier) :into funcs
     :finally (report.write-report stream headers funcs (collect-storage storage-specifier))))

(defun report.%rsc (symbol func)
  (report.register-standard-column symbol func))

;; register standard columns
(defun report.register-standart-columns ()
  ;; product functions
  (report.%rsc 'product-articul #'articul)
  (report.%rsc 'product-price #'price)
  (report.%rsc 'product-siteprice #'siteprice)
  (report.%rsc 'product-name #'name-provider)
  (report.%rsc 'product-name-real #'name-seo)
  (report.%rsc
   'product-yml-name
   (rcurry #'get-option "Secret" "Yandex"))
  (report.%rsc 'product-yml-show #'yml.yml-show-p)
  (report.%rsc
   'product-seo-text-exists
   #'(lambda (item) (if (valid-string-p (seo-text item)) "есть" "нет")))
  (report.%rsc
   'product-num-pics
   #'(lambda (item) (length (get-pics (articul item)))))
  (report.%rsc 'product-valid-options #'valid-options)
  (report.%rsc
   'product-active
   #'(lambda (item) (if (active item) "да" "нет")))
  (report.%rsc
   'product-group
   #'(lambda (item) (when (parent item) (name (parent item)))))
  (report.%rsc
   'product-grandparent
   #'(lambda (item) (when (and (parent item) (parent (parent item)))
                      (name (parent (parent item))))))
  ;; return name of 2 level group(counting from root, root group has 1 level),
  ;; which is ancestor of given item
  (report.%rsc
   'product-2-lvl-group
   #'(lambda (item) (loop
                       :for cur := (parent item) :then (parent cur)
                       :while (and cur (parent cur) (parent (parent cur)))
                       :finally (return (when (and cur (parent cur)) (name cur))))))
  (report.%rsc
   'product-secret (rcurry #'get-option "Secret" "Checked"))
  (report.%rsc
   'product-dtd
   #'(lambda (item)
       (gethash (articul item) *xls.product-table*)))
  (report.%rsc 'product-vendor #'vendor)
  (report.%rsc 'product-delivery #'yml.get-product-delivery-price1)
  (report.%rsc
   'product-seria (rcurry #'get-option "Общие характеристики" "Серия"))
  (report.%rsc
   'product-direct-name (rcurry #'get-option "Secret" "Direct-name"))
  (report.%rsc
   'product-double (rcurry #'get-option "Secret" "Дубль"))
  (report.%rsc
   'product-warranty (rcurry #'get-option "Дополнительная информация" "Гарантия"))
  (report.%rsc
   'product-url
   #'(lambda (item)
       ;; TODO: use restas url designator
       (format nil "http://www.320-8080.ru/~A" (key item))))
;;; group functions
  (report.%rsc
   'group-name #'name)
  (report.%rsc
   'group-url
   #'(lambda (item)
       ;; TODO: use restas url designator
       (format nil "http://www.320-8080.ru/~A" (key item))))
  (report.%rsc
   'group-active #'(lambda (item) (if (active item) "yes" "no")))
  (report.%rsc
   'group-seo-text
   #'(lambda (item)
       (if (valid-string-p (seo-text item))
           "yes" "no")))
  (report.%rsc
   'group-count-products
   #'(lambda (item)
       (length (products item))))
  (report.%rsc
   'group-count-active-products
   #'(lambda (item)
       (count-if #'active (products item)))))

(report.register-standart-columns)

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
         (cons "seo текст" 'product-seo-text-exists)
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
         (cons "активных" 'group-count-active-products))
   'group))

(defun report.product-vendor-report (stream)
  (report.write-report-with-standard-columns
   stream
   (list (cons "Название категории" 'product-group)
         (cons "Брэнд" 'product-vendor)
         (cons "Название товара" 'product-name-real)
         (cons "url страницы" 'product-url)
         (cons "Active" 'product-active)
         (cons "seo-text" 'product-seo-text-exists))
   'product))

(defun report.pics-report (stream &optional (products nil products-supplied-p))
  (report.write-header stream
                       (list "Продукт"
                             "имя"
                             "ширина"
                             "высота"
                             "размер (Кб)"))
  (mapcar
   #'(lambda (product)
       (mapcar
        #'(lambda (pic)
            (let* ((pic-path (pic-path (key product) pic))
                   (dimensions (get-dimensions pic-path))
                   (size (with-open-file (file pic-path) (file-length file))))
              (format stream "~{~A;~}~%"
                      (list (key product)
                            pic
                            (getf dimensions :width)
                            (getf dimensions :height)
                            (floor size 1000))))) ; in Kb
        (get-pics (key product))))
   (if products-supplied-p
       products
       (collect-storage 'product)))
  (values))

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
                                (if (classes.has-vendor-seo-text v vendor)
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
           "555555"
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
         (when (and
                (active v)
                (not (preorder v))
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
    (create-report name #'report.product-vendor-report)))


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

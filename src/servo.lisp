;;;; servo.lisp

(in-package #:eshop)

(defmacro with-sorted-paginator (get-products request-get-plist body)
  `(let* ((products ,get-products)
          (sorting  (getf ,request-get-plist :sort))
          (sorted-products (string-case sorting
                             ("pt" (sort products #'< :key #'siteprice))
                             ("pb" (sort products #'> :key #'siteprice))
                             (t products))))
     (multiple-value-bind (paginated pager)
         (paginator ,request-get-plist sorted-products)
       ,body)))


(defmacro sorts (request-get-plist)
  `(let ((variants '(:pt "увеличению цены" :pb "уменьшению цены"))
         (url-parameters ,request-get-plist))
     (remf url-parameters :page)
     (remf url-parameters :sort)
     (loop :for sort-field :in variants :by #'cddr :collect
        (let ((key (string-downcase (format nil "~a" sort-field))))
          (setf (getf url-parameters :sort) key)
          (if (string= (string-downcase (format nil "~a" sort-field))
                       (getf ,request-get-plist :sort))
              (list :key key
                    :name (getf variants sort-field)
                    :url (servo.make-get-str url-parameters)
                    :active t)
              (list :key key
                    :url (servo.make-get-str url-parameters)
                    :name (getf variants sort-field)))))))


(defmethod rightblocks ((object group) (parameters list))
  (list (soy.catalog:rightblock1)
        (soy.catalog:rightblock2)
        (if (not (groupp object))
            ""
            (soy.catalog:seotext
             (list :text
                   (classes.get-group-seo-text
                    object
                    (getf parameters :vendor)))))))

(defmacro f-price ()
  `(lambda (product request-plist filter-options)
     (let ((value-f (getf request-plist :price-f))
           (value-t (getf request-plist :price-t))
           (value-x (siteprice product)))
       (unless value-f
         (setf value-f "0"))
       (unless (valid-string-p value-t)
         (setf value-t "99999999"))
       (setf value-f (arnesi:parse-float (format nil "~as" value-f)))
       (setf value-t (arnesi:parse-float (format nil "~as" value-t)))
       (<= value-f value-x value-t))))


(defmacro with-range (key optgroup-name option-name)
  `(lambda (product request-plist filter-options)
     (let ((value-f (getf request-plist (intern (string-upcase (format nil "~a-f" (symbol-name ,key))) :keyword)))
           (value-t (getf request-plist (intern (string-upcase (format nil "~a-t" (symbol-name ,key))) :keyword)))
           (value-x 0))
       ;; TOCHECK
       (setf value-x (get-option product
                                 ,optgroup-name ,option-name))
       (when (null value-x)
         (setf value-x "0"))
       (when (null value-f)
         (setf value-f "0"))
       (when (or (null value-t)
                 (string= value-t ""))
         (setf value-t "99999999"))
       (setf value-f (arnesi:parse-float (format nil "~as" value-f)))
       (setf value-t (arnesi:parse-float (format nil "~as" value-t)))
       (setf value-x (arnesi:parse-float (format nil "~as" value-x)))
       (and (<= value-f value-x value-t)))))

;;Фильтруем по наличию опции
(defun filter-with-check-options (key-name option-group-name product request-plist filter-options)
  (let ((number 0)
        (result-flag t))
    (mapcar #'(lambda (option-name)
                (let ((value-p (getf request-plist
                                     (intern (string-upcase
                                              (format nil "~a-~a"
                                                      key-name
                                                      number))
                                             :keyword))))
                  (incf number)
                  (when (equal value-p "1")
                    (let ((value-x))
                      (mapcar #'(lambda (optgroup)
                                  (if (string= (getf optgroup :name) option-group-name)
                                      (progn
                                        (let ((options (getf optgroup :options)))
                                          (mapcar #'(lambda (option)
                                                      (if (string= (getf option :name) option-name)
                                                          (setf value-x (getf option :value))))
                                                  options)))))
                              (optgroups product))
                      (unless (string= value-x "Есть")
                        (setf result-flag nil))))))
            filter-options)
    result-flag))

;;фильтрация по значениям опции
(defun filter-with-check-values (key-name option-group-name option-name product request-plist filter-options)
  (let ((number 0)
        (result-flag nil)
        (request-flag t)
        (value-x
         (get-option product
                     option-group-name option-name)))
    (mapcar #'(lambda (option-value)
                (let ((value-p (getf request-plist
                                     (intern (string-upcase
                                              (format nil "~a-~a"
                                                      key-name
                                                      number))
                                             :keyword))))
                  (incf number)
                  (when (equal value-p "1")
                    (setf request-flag nil)
                    (when (string= value-x option-value)
                        (setf result-flag t)))))
            filter-options)
    (or result-flag
        request-flag)))

(defmacro with-check (key optgroup-name dummy-var)
  `(lambda (product request-plist filter-options)
     (let ((option-group-name ,optgroup-name)
           (key-name (symbol-name ,key)))
       (if (string= ,dummy-var "")
           (filter-with-check-options key-name option-group-name product request-plist filter-options)
           (filter-with-check-values key-name option-group-name ,dummy-var product request-plist filter-options)))))


(defmacro with-radio (key optgroup-name option-name)
  `(lambda (product request-plist filter-options)
     (let ((value-p (getf request-plist (intern (string-upcase (format nil "~a" (symbol-name ,key))) :keyword)))
           (value-x ""))
       (with-option1 product
         ,optgroup-name ,option-name
         (setf value-x (getf option :value)))
       (cond
         ((null value-p)
          t)
         ((null value-x)
          nil)
         (t
          (progn
            (setf value-p (parse-integer value-p))
            (let ((opt-val (nth value-p filter-options)))
              (if (string= opt-val "Любой")
                  t
                  (string= value-x opt-val)))))))))

(defun paginator-page-line (request-get-plist start stop current)
  (loop :for i from start :to stop :collect
     (let ((plist request-get-plist)
           (is-current-page nil))
       (setf (getf plist :page) (format nil "~a" i))
       (setf is-current-page (= current i))
       (format nil "<a href=\"?~a\">~:[~;<big><b>~]~a~:[~;</b></big>~]</a>"
               (servo.make-get-str plist)
               is-current-page
               i
               is-current-page))))

(defun paginator (request-get-plist sequence &optional (pagesize 15))
  (let ((page (getf request-get-plist :page))
        (page-count (ceiling (length sequence) pagesize)))
    (when (null page)
      (setf page "1"))
    (setf page (parse-integer page :junk-allowed t))
    (unless (and (numberp page)
                 (plusp page))
      (setf page 1))
    (if (> page page-count)
        (setf page page-count))
    (let* ((result (let ((tmp (ignore-errors (subseq sequence (* pagesize (- page 1))))))
                     (when (> (length tmp) pagesize)
                       (setf tmp (subseq tmp 0 pagesize)))
                     tmp))
           (start-page-line nil)
           (cur-page-line nil)
           (stop-page-line nil)
           (start-number 1)
           (stop-number page-count)
           (page-line-string ""))
      (if (> page 5)
          (progn
            (setf start-number (- page 2))
            (setf start-page-line (paginator-page-line request-get-plist 1 2 0))))
      (if (> (- page-count page) 5)
          (progn
            (setf stop-number (+ page 2))
            (setf stop-page-line (paginator-page-line request-get-plist (- page-count 1) page-count 0))))
      (setf cur-page-line (paginator-page-line request-get-plist start-number stop-number page))
      (if (> page-count 1)
          (setf page-line-string
                (format nil "~@[~{~a~}...~] ~{~a ~} ~@[...~{~a~}~]"
                        start-page-line
                        cur-page-line
                        stop-page-line)))
      (values result page-line-string))))


(defun default-page (&optional (content nil) &key keywords description title no-need-cart)
  (soy.index:main (list :keywords keywords
                        :description description
                        :title title
                        :header (soy.header:header (append (list :cart (unless no-need-cart
                                                                         (soy.index:cart))
                                                                 :bannertype "line-text")
                                                           (main-page-show-banner "line-text" (banner *main-page.storage*))))
                        :content (if content
                                     content
                                     (format nil "<pre>'~a' ~%'~a' ~%'~a'</pre>"
                                             (request-str)
                                             (hunchentoot:request-uri *request*)
                                             (hunchentoot:header-in* "User-Agent"))))))


(defun request-str ()
  (let* ((request-full-str (hunchentoot:url-decode (hunchentoot:request-uri hunchentoot:*request*)))
         (request-parted-list (split-sequence:split-sequence #\? request-full-str))
         (request-str (string-right-trim "\/" (car request-parted-list)))
         (request-list (split-sequence:split-sequence #\/ request-str))
         (request-get-plist (when (cadr request-parted-list)
                              (let (result)
                                (loop
                                   :for param :in (split-sequence:split-sequence #\& (cadr request-parted-list))
                                   :do (let ((split (split-sequence:split-sequence #\= param)))
                                         (setf (getf result (intern (string-upcase (car split)) :keyword))
                                               (if (null (cadr split))
                                                   ""
                                                   (cadr split)))))
                                result))))
    (values request-str request-list request-get-plist)))


(defun request-get-plist ()
  (multiple-value-bind (request-str request-list request-get-plist)
      (request-str)
    request-get-plist))


(defun request-list ()
  (multiple-value-bind (request-str request-list request-get-plist)
      (request-str)
    request-list))


(defun servo.make-get-str (request-get-plist)
  "Convert plist of get params to url (without encoding)"
  (format nil "~{~(~A~)=~A~^&~}" request-get-plist))

(defun strip ($string)
  (cond ((vectorp $string) (let (($ret nil))
                             (loop
                                for x across $string collect x
                                do (if (not
                                        (or
                                         (equal x #\')
                                         (equal x #\")
                                         (equal x #\!)
                                         (equal x #\%)
                                         (equal x #\\)
                                         (equal x #\/)
                                         ))
                                       (push x $ret)))
                             (coerce (reverse $ret) 'string)))
        ((listp $string)   (if (null $string)
                               ""
                               $string))))

(defun stripper ($string)
  (cond ((vectorp $string) (let (($ret nil))
                             (loop
                                for x across $string collect x
                                do (if (not
                                        (or
                                         (equal x #\')
                                         (equal x #\")
                                         (equal x #\\)
                                         (equal x #\~)
                                         (equal x #\Newline)))
                                       (push x $ret)))
                             (let ((ret (coerce (reverse $ret) 'string)))
                               (when (equal 0 (length ret))
                                 (return-from stripper ""))
                               ret)))))


(defun replace-all (string part replacement &key (test #'char=))
  "Returns a new string in which all the occurences of the part
 is replaced with replacement."
  (with-output-to-string (out)
    (loop :with part-length := (length part)
       :for old-pos := 0 :then (+ pos part-length)
       :for pos := (search part string
                           :start2 old-pos
                           :test test)
       :do (write-string string out
                         :start old-pos
                         :end (or pos (length string)))
       :when pos :do (write-string replacement out)
       :while pos)))


(defun get-format-price (p)
  (format nil "~,,' ,3:d" p))


;; выбор нескольких случайных элементов из списка
;; если количество не указано то возвращается список из одного элемента
;; если количество больше длинны входного списка, то возвращается перемешанный входной список
(defun get-randoms-from-list (elts &optional (count 1))
  ;;уменьшаем count до длинны списка если надо
  (when (< (length elts) count)
    (setf count (length elts)))
  (loop
     :for n :from 1 :to count
     :collect (let* ((pos (random (length elts)))
                     (element (nth pos elts)))
                (setf elts (remove element elts :count 1))
                element)))

(defmethod get-filter-function-option (malformed-filter-list)
  (let ((option nil))
    (maplist #'(lambda (val)
                 (when (or (equal (car val) :radio)
                           (equal (car val) :checkbox))
                   (setf option (cadr val))))
             malformed-filter-list)
    option))

;; TODO: удалить из кода
(defmethod filter-controller ((object group) request-get-plist)
  (let ((functions (mapcar #'(lambda (elt)
                               (cons (eval (car (last elt)))
                                     (get-filter-function-option elt)))
                           (base (fullfilter object)))))
    (mapcar #'(lambda (filter-group)
                (let ((advanced-filters (cadr filter-group)))
                  (mapcar #'(lambda (advanced-filter)
                              (nconc functions (list (cons (eval (car (last advanced-filter)))
                                                           (get-filter-function-option advanced-filter)))))
                          advanced-filters)))
            (advanced (fullfilter object)))
    ;; processing
    (let ((result-products))
      (mapcar #'(lambda (product)
                  (when (loop
                           :for function :in functions
                           :finally (return t)
                           :do (unless (funcall (car function)
                                                product
                                                request-get-plist
                                                (cdr function))
                                 (return nil)))
                    (push product result-products)))
              (remove-if-not #'(lambda (product)
                                 (active product))
                             (storage.get-recursive-products object)))
      result-products)))

(defmethod fullfilter-controller (product-list (object group) request-get-plist)
  (let ((functions (mapcar #'(lambda (elt)
                               (cons (eval (car (last elt)))
                                     (get-filter-function-option elt)))
                           (base (fullfilter object)))))
    (mapcar #'(lambda (filter-group)
                (let ((advanced-filters (cadr filter-group)))
                  (mapcar #'(lambda (advanced-filter)
                              (nconc functions (list (cons (eval (car (last advanced-filter)))
                                                           (get-filter-function-option advanced-filter)))))
                          advanced-filters)))
            (advanced (fullfilter object)))
    ;; processing
    (let ((result-products))
      (mapcar #'(lambda (product)
                  (when (loop
                           :for function :in functions
                           :finally (return t)
                           :do (unless (funcall (car function)
                                                product
                                                request-get-plist
                                                (cdr function))
                                 (return nil)))
                    (push product result-products)))
              product-list)
      result-products)))

(defun url-to-request-get-plist (url)
  (let* ((request-full-str url)
         (request-parted-list (split-sequence:split-sequence #\? request-full-str))
         (request-get-plist (let ((result))
                              (loop :for param :in (split-sequence:split-sequence #\& (cadr request-parted-list)) :do
                                 (let ((split (split-sequence:split-sequence #\= param)))
                                   (setf (getf result (intern (string-upcase (car split)) :keyword))
                                         (if (null (cadr split))
                                             ""
                                             (cadr split)))))
                              result)))
    request-get-plist))



(defun if-need-to-show-hidden-block (elt request-get-plist)
  (let ((key (string-downcase (format nil "~a" (nth 0 elt))))
        (showflag nil))
    ;; проверку нужно ли раскрывать hidden блока
    (cond
      ((equal :radio (nth 2 elt))
       (loop
          :for nameelt
          :in  (nth 3 elt)
          :for i from 0
          :do (if (string= (format nil "~a" i)
                           (getf request-get-plist (intern
                                                    (string-upcase key))))
                  (setf showflag t))))
      ((equal :checkbox (nth 2 elt))
       (loop
          :for nameelt
          :in  (nth 3 elt)
          :for i from 0
          :do (if  (string= "1" (getf request-get-plist (intern
                                                         (string-upcase
                                                          (format nil "~a-~a" key i))
                                                         :keyword)))
                   (setf showflag t)))))
    showflag))


(defun is-need-to-show-advanced (fullfilter request-get-plist)
  (let ((flag nil))
    (mapcar #'(lambda (elt)
                (mapcar #'(lambda (inelt)
                            (setf flag (or flag
                                           (if-need-to-show-hidden-block inelt request-get-plist))))
                        (cadr elt)))
            (advanced fullfilter))
    flag))

(defun filter-element (elt request-get-plist)
  (let* ((key (string-downcase (format nil "~a" (nth 0 elt))))
         (name (nth 1 elt))
         (showflag nil)
         (ishidden (search '(:hidden) elt))
         (start (search '(:start) elt))
         (end (search '(:end) elt))
         (step (search '(:step) elt))
         (contents
          (cond ((equal :range (nth 2 elt))
                 (soy.fullfilter:range
                  (list :unit (nth 3 elt)
                        :key key
                        :name name
                        :ishidden ishidden
                        :start start
                        :end end
                        :step step
                        :from (getf request-get-plist
                                    (intern (string-upcase (format nil "~a-f" key)) :keyword))
                        :to (getf request-get-plist
                                  (intern (string-upcase (format nil "~a-t" key)) :keyword)))))
                ((equal :slider (nth 2 elt))
                 (soy.fullfilter:slider
                  (list :unit (nth 3 elt)
                        :key key
                        :name name
                        :ishidden ishidden
                        :from (getf request-get-plist
                                    (intern (string-upcase (format nil "~a-f" key)) :keyword))
                        :to (getf request-get-plist
                                  (intern (string-upcase (format nil "~a-t" key)) :keyword)))))
                ((equal :radio (nth 2 elt))
                 (soy.fullfilter:box
                  (list :key key
                        :name name
                        :ishidden ishidden
                        :elts (let ((elts (nth 3 elt)))
                                (loop :for nameelt :in elts
                                   :for i :from 0 :collect
                                   (soy.fullfilter:radioelt
                                    (list :key key
                                          :value i
                                          :name nameelt
                                          :checked (string= (format nil "~a" i)
                                                            (getf request-get-plist (intern
                                                                                     (string-upcase key)
                                                                                     :keyword))))))))))
                ((equal :checkbox (nth 2 elt))
                 (soy.fullfilter:box
                  (list :key key
                        :name name
                        :ishidden ishidden
                        :elts (let ((values (nth 3 elt)))
                                (loop :for value :in values
                                   :for i from 0 :collect
                                   (soy.fullfilter:checkboxelt
                                    (list :value value
                                          :key key
                                          :i i
                                          :checked (string= "1" (getf request-get-plist (intern
                                                                                         (string-upcase
                                                                                          (format nil "~a-~a" key i))
                                                                                         :keyword))))))))))
                (t ""))))
    (if ishidden
        (soy.fullfilter:hiddencontainer (list :key key
                                              :name name
                                              :contents contents
                                              :isshow (if-need-to-show-hidden-block elt request-get-plist)))
        contents)))


(defmethod vendor-controller ((object group) vendor)
  (let* ((result-products))
    (mapcar #'(lambda (product)
                (let ((vendor (vendor product)))
                  (if (string=
                       (string-downcase (string-trim '(#\Space #\Tab #\Newline) vendor))
                       (string-downcase (string-trim '(#\Space #\Tab #\Newline)
                                                     (ppcre:regex-replace-all "%20" vendor " "))))
                      (push product result-products))))
            (storage.get-recursive-products object))
    result-products))

;; TODO: move to filters
(defun vendor-filter-controller (product vendor)
  (let ((product-vendor (vendor product)))
    (string=
     (string-downcase (string-trim '(#\Space #\Tab #\Newline) product-vendor))
     (string-downcase (string-trim '(#\Space #\Tab #\Newline)
                                   (ppcre:regex-replace-all "%20" vendor " "))))))

;;; Функция, добавляющая в хлебные крошки вендора, если он присутствует
;;; в get запросе.
(defun breadcrumbs-add-vendor1 (breadcrumbs parameters)
  (let ((belts (getf breadcrumbs :breadcrumbelts))
        (tail (getf breadcrumbs :breadcrumbtail))
        (vendor (vendor-transform-from-alias (getf parameters :vendor))))
    (if vendor
        (list :breadcrumbelts (append belts (list tail))
              :breadcrumbtail (list :key vendor
                                    :val (format nil "~a ~a"
                                                 (getf tail :val)
                                                 vendor)))
        ;; else
        breadcrumbs)))

(defun string-titlecase (title)
  "Make string starting with capital letter"
  (if title
      (format nil "~@(~A~)" title)
      ""))


(defun servo.compile-soy (&rest tmpl-name)
  (mapcar #'(lambda (fname)
              (let ((pathname (merge-pathnames (pathname fname) (config.get-option "PATHS" "path-to-templates"))))
                (log5:log-for info "compile template: ~a" pathname)
                (closure-template:compile-template :common-lisp-backend pathname)))
          tmpl-name))

(defun anything-to-keyword (anything)
  "Convert anything that has print method to keyword; Case insensitive"
  (intern (format nil "~:@(~A~)" anything) :keyword))

(defun anything-to-symbol (anything &optional (package (find-package :eshop)))
  "Convert anything that has print method to symbol; Case insensitive"
  (intern (format nil "~:@(~A~)" anything) package))

(defun alistp (obj)
  "Checks whether object is association list (e.g. list of conses)"
  (when (listp obj) (every #'consp obj)))

(deftype alist ()
  `(satisfies alistp))

(defun servo.alist-to-plist (alist)
  "Non-recursive convertion from association list to property list"
  (declare (alist alist))
  (loop
     :for (key . value)
     :in alist
     :collect (anything-to-keyword key)
     :collect value))

(defun servo.recursive-alist-to-plist (alist)
  "Rcursive convertion from association list to property list"
  (declare (alist alist))
  (loop
     :for (key . value)
     :in alist
     :collect (anything-to-keyword key)
     :collect (if (alistp value)
                  (servo.recursive-alist-to-plist value)
                  value)))

(defun servo.plist-to-unique (plist)
  "Remove duplacating keys"
  (let ((result))
    (loop
       :while plist
       :do (let ((key (car plist))
                 (value (cadr plist)))
             (setf (getf result key)
                   (aif (getf result key)
                        (append
                         (ensure-list it)
                         (list value))
                       ;;else
                        value)))
       (setf plist (cddr plist)))
    result))

(defun servo.list-to-hashtasble (list)
  "Converting list (hash1 val1 hash2 val2...) to hashtable"
  ;; (sb-pcl::doplist
  (let ((res (make-hash-table :test #'equal)))
    (loop
       :for x :from 0 :to (- (length list) 1)
       :when (evenp x)
       :do
       (let ((key (nth x list))
             (value (nth (+ 1 x) list)))
         (setf (gethash key res) value)))
    res))

(defun servo.diff-percentage (full part)
  "Returns difference in percents. (1 - part / full) * 100%"
  (when (and full part (not (zerop full)))
    (format nil "~1$" (* (- 1 (/ part full)) 100))))

(defun servo.diff-price (product-1 product-2)
  (if (plusp (siteprice product-1))
      (abs (/ (- (siteprice product-1) (siteprice product-2))
              (siteprice product-1)))
      ;; else infinity
      999999))

(defun servo.get-product-vendor (product)
  (get-option product
              "Общие характеристики"
              "Производитель"))

(defun servo.find-relative-product-list (product &optional (coef 2))
  "Returns list of products from same group with similar vendor"
  (when (parent product)
    (let* ((vendor (vendor product))
           (diff-list
            (remove-if #'(lambda (v) (or (equal 0 (siteprice (cdr v)))
                                         (not (active (cdr v)))))
                       (mapcar #'(lambda (a)
                                   (let ((diff
                                          (if (equal (vendor a) vendor)
                                              1
                                              coef)))
                                     (cons (* diff (servo.diff-price product a))
                                           a)))
                               (copy-list (products (parent product)))))))
      (mapcar #'cdr
              (sort diff-list
                    #'(lambda (a b)
                        (< (car a) (car b))))))))

(defun servo.edit-get-param (url name value)
  (let* ((uri (puri:parse-uri url))
         (params (hunchentoot::form-url-encoded-list-to-alist (split "&" (puri:uri-query uri))))
         (is-new-param t)
         (query))
    (mapcar #'(lambda (p)
                (when (equal (car p) name)
                  (setf (cdr p) value)
                  (setf is-new-param nil)))
            params)
    (if is-new-param
        (push (cons name value) params))
    (setf (puri:uri-query uri) (format nil "~{~a~^&~}"
                                       (mapcar #'(lambda (p)
                                                   (concatenate 'string (car p)
                                                                (if (not (equal (cdr p) ""))
                                                                    (format nil "=~a" (cdr p)))))
                                               params)))
    (format nil "~a" uri)))

(defun servo.string-replace-chars (string char-list &key (replacement nil))
  "Replacing all chars in char-list from string"
  (coerce
   (remove-if #'null
              (map 'list (lambda (c)
                           (if (some #'(lambda (c1) (char= c c1))
                                     char-list)
                               replacement
                               c))
                   string))
   'string))

(defun valid-string-p (s &key (whitespace-check t)
                              (unwanted-chars (list #\Space #\Tab #\Newline))
                              (del-method :replace-all))
  (and s (string/= s "") (if whitespace-check
                             (string/= ""
                                       (case del-method
                                         (:replace-all (servo.string-replace-chars s unwanted-chars))
                                         (:trim (string-trim unwanted-chars s))
                                         (:left-trim (string-left-trim unwanted-chars s))
                                         (:right-trim (string-right-trim unwanted-chars s))))
                             t)
       ;; for returning t if valid (not number)
       t))

(defun ensure-list (obj)
  "When obj is list return obj, otherwise return list with only element - obj"
  (if (consp obj) obj (list obj)))

(defun ensure-string (obj)
  "If obj is string return obj, if obj is nil, convert to empty string, otherwise throw error"
  ;; TODO: add printable types check
  (etypecase obj
    (string obj)
    (null "")
    (t (error "Object is niether string nor nil"))))

(defun htmlize (string)
  "Make string appropriate for viewing in html (replace newlines with <br />);
Used for printing system info to browser"
  (declare (string string))
  (regex-replace-all "\\n" string "<br />"))

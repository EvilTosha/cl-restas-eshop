(in-package #:eshop)


(defun write-products-report (stream)
  (format stream "~a;~a;~a;~a;~a;~a;~a;~a;~a;~a;~a;~a;~a;~a;~a;~a;~a;~%"
          "артикул" "цена магазина" "цена сайта" "имя" "имя real" "имя yml" "is-yml-show" "seo текст"
          "фотографии" "характеристики" "активный" "группа" "родительская группа"
          "secret" "DTD" "vendor" "доставка")
  (maphash #'(lambda (k v)
               (declare (ignore k))
               ;; (wlog stream)
               (let ((id "нет") (name "нет") (name-real "нет") (name-yml "нет")
                     (desc "нет") (img "нет") (options "нет") (active "нет")
                     (group-name "нет") (parent-group-name "нет") (secret "нет"))
                 (when (equal (type-of v)
                              'product)
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
                   (setf group-name (if (not (null (new-classes.parent v)))
                                        (stripper (name (new-classes.parent v)))))
                   (setf parent-group-name (if (and (not (null (new-classes.parent v)))
                                                    (not (null (new-classes.parent (new-classes.parent v)))))
                                               (stripper (name (new-classes.parent (new-classes.parent v))))))
                   (setf secret "Нет")
                   (with-option1 v "Secret" "Checked"
                                 (setf secret (getf option :value)))
                   (if (string= (format nil "~a" id) "172466")
                       (wlog v))
                   (format stream "~a;~a;~a;\"~a\";\"~a\";\"~a\";~a;~a;~a;~a;~a;\"~a\";\"~a\";~a;~a;~a;~a~%"
                           id (price v) (siteprice v) name name-real
                           name-yml (yml.is-yml-show v) desc img options active group-name
                           parent-group-name secret
                           (gethash (articul v) *xls.product-table*)
                           (vendor v)
                           (yml.get-product-delivery-price1 v))
                   )))
           (storage *global-storage*)))

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
                                        ;; (print (format nil "~a:~a" (getf v :name) (getf l :value)))
                                        (setf flag t)))
                       (getf v :options)))
        (optgroups product))
  flag
  ))

(defun write-groups (stream)
  (format stream "~a;~a;~a;~a;~%"
          "Название категории"
          "url страницы"
          "Active"
          "seo-text")
  (maphash #'(lambda (k v)
               (declare (ignore k))
               (when (equal (type-of v)
                              'group)
                 (format stream "\"~a\";http://www.320-8080.ru/~a;~a;~a;~%"
                           (stripper (name v))
                           (key v)
                           (if (active v)
                               "yes"
                               "no")
                           (if (and (not (null (seo-text v)))
                                       (not (string= "" (stripper (seo-text v)))))
                                  "yes"
                                  "no"))
                   ))
           (storage *global-storage*)))

(defun write-groups-active-product-num (stream)
  (format stream "~a;~a;~a;~a;~%"
          "Название категории"
          "url страницы"
          "Active"
          "кол-во товаров")
  (maphash #'(lambda (k v)
               (declare (ignore k))
               (when (equal (type-of v)
                              'group)
                 (format stream "\"~a\";http://www.320-8080.ru/~a;~a;~a;~%"
                           (stripper (name v))
                           (key v)
                           (if (active v)
                               "yes"
                               "no")
                           (length (remove-if-not #'active (get-recursive-products v))))))
           (storage *global-storage*)))



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
    (maphash #'(lambda (k v)
                 (declare (ignore k))
                 (when (equal (type-of v)
                              'product)
                   (setf vendor-name "Нет")
                   (setf vendor-name (vendor v))
                   (setf desc (if (and (not (null (seo-text v)))
                                       (not (string= "" (stripper (seo-text v)))))
                                  "yes"
                                  "no"))
                   (format stream "\"~a\";\"~a\";\"~a\";http://www.320-8080.ru/~a;~a;~a;~%"
                           (if (not (null (new-classes.parent v)))
                               (stripper (name (new-classes.parent v)))
                               "Нет категории")
                           (stripper vendor-name)
                           (stripper (name-seo v))
                           (articul v)
                           (if (active v)
                               "yes"
                               "no")
                           desc)))
             (storage *global-storage*))))


(defun write-vendors (stream)
  (format stream "~a;~a;~a;~a;~a;~%"
          "Название категории"
          "Брэнд"
          "url страницы"
          "Active"
          "seo-text")
  (maphash #'(lambda (k v)
               (declare (ignore k))
               (when (and (equal (type-of v)
                            'group)
                          (null (groups v)))

                 (maphash #'(lambda (vendor num)
                              (declare (ignore num))
                              (format stream "\"~a\";\"~a\";http://www.320-8080.ru/~a?vendor=~a;~a;~a;~%"
                                      (stripper (name v))
                                      (stripper vendor)
                                      (hunchentoot:url-encode (key v))
                                      (hunchentoot:url-encode (stripper vendor))
                                       "yes"
                                       (let ((desc (gethash (string-downcase vendor) (vendors-seo v))))
                                         (if (and (not (null desc))
                                                  (not (string= "" desc)))
                                             "yes"
                                             "no"))))
                           ;; (producersall (make-producers v)))
                           (storage.get-vendors (storage.get-filtered-products v #'atom)))
                 ))
           (storage *global-storage*)))

(defun create-report (file-name report-func)
  (let ((filename (format nil "~a/~a" *path-to-dropbox* file-name)))
    (with-open-file
        (stream filename :direction :output :if-exists :supersede :external-format :cp1251)
      (print stream)
      (funcall report-func stream)
      )))


(defun check-valid-siteprice ()
  (format t "~&~a;\"~a\";~a;~a;~a;"
          "Артикул"
          "Имя"
          "Активный"
          "Цена магазина"
          "Цена 3208080")
  (maphash #'(lambda(k v)
               (declare (ignore k))
               (when (equal (type-of v)
                            'product)
                 (if (< (price v)
                        (siteprice v))
                     (format t "~&~a;\"~a\";~a;~a;~a;"
                             (articul v)
                             (stripper (name v))
                             (if (active v)
                                 "yes"
                                 "no")
                             (price v)
                             (siteprice v)))))
           (storage *global-storage*)))

(defun show-last-history (stream)
  (when (not (null *history*))
    ;; Делаем все продукты неактивными
    (maphash #'(lambda (k v)
                 (declare (ignore k))
                 (when (equal (type-of v) 'product)
                   (setf (active v) nil)))
             (storage *global-storage*))
    (loop :for packet :in (reverse (caddr (car *history*))) :do
       (format stream "~a" (sb-ext:octets-to-string packet :external-format :cp1251)))))





(defun error-price-report ()
  (let ((products (remove-if-not #'(lambda (v) (< (price v) (siteprice v)))
                                 (remove-if-not #'eshop::active (wolfor-stuff::get-products-list)))))
    (when products
      (gateway-send-error-mail (list "web_design@alpha-pc.com"
                                     "wolforus@gmail.com"
                                     "slamly@gmail.com")
                               (format nil "~&Цена на сайте выше цены в магазине: ~a<br/>~{~&~a<br/>~}"
                                       (length products)
                                       (mapcar #'(lambda (v)
                                                   (format nil "~&<a href=\"http://www.320-8080.ru/~a\">~a</a>:~a | siteprice:~a price:~a"
                                                           (articul v)
                                                     (articul v)
                                                     (name v)
                                                     (siteprice v)
                                                     (price v)))
                                               products))
                               "Siteprice > Price"))))


                          ;; (setf (active (gethash "160420" (storage *global-storage*))) nil)
                          ;; (serialize (gethash "160420" (storage *global-storage*)))
                          ;; (setf (active (gethash "165359" (storage *global-storage*))) nil)
                          ;; (serialize (gethash "165359" (storage *global-storage*)))
                          ;; (setf (active (gethash "165360" (storage *global-storage*))) nil)
                          ;; (serialize (gethash "165360" (storage *global-storage*)))
;; (setf (active (gethash "157499" (storage *global-storage*))) nil)
;; (serialize (gethash "157499" (storage *global-storage*)))
;; (setf (active (gethash "153599" (storage *global-storage*))) nil)
;; (serialize (gethash "153599" (storage *global-storage*)))



(defparameter *special-products* (make-hash-table :test #'equal))



(defun post-proccess-gateway ()
    (mapcar #'(lambda (v)
                (let ((p (gethash v (storage *global-storage*)))
                  ;; (p (make-instance 'product
                  ;;                   :key v
                  ;;                   :articul v))
                  )
              ;; (if p1
                  ;; (setf p p1))
              (when (not (null p))
                  (setf (preorder p) t)
                  (setf (active p) t)
                  ;; (serialize p)
                  (setf (gethash v (storage *global-storage*)) p)
                  (setf (gethash v *special-products*) p))))
        (list
         "999888"
         "711265"
         "834786"
         "938111"
         "777888"
         "888777"
         "999111"
         "999777"
         ))
  (let ((rs))
    (maphash #'(lambda (k v)
                 (declare (ignore k))
                 (when (and (equal (type-of v) 'product)
                            (active v)
                            (= (siteprice v) 0))
                   (push v rs)
                   (setf (active v) nil)
                   (wlog (key v))))
             (storage *global-storage*))
    (length rs)))

;; (mapcar #'(lambda (v) (setf (active v) nil))
;;  (storage.get-filtered-products (gethash "vinchester" (storage *global-storage*))))
;; (setf (active (gethash "vinchester" (storage *global-storage*))) nil)
;; (mapcar #'(lambda (v) (setf (active v) nil))
;;  (storage.get-filtered-products (gethash "vneshnie-zhostkie-diski" (storage *global-storage*))))
;; (setf (active (gethash "vneshnie-zhostkie-diski" (storage *global-storage*))) nil)


  ;; (maphash #'(lambda (k v)
  ;;              (declare (ignore k))
  ;;              (when (equal (type-of v)
  ;;                             'group)
  ;;                (when (not (delivery-price v))
  ;;                    (wlog (name v))
  ;;                    (setf (delivery-price v) 300))))
  ;;          (storage *global-storage*))



(defun product-delivery (p)
  (let ((g (parent p))
        (daily (gethash (articul p) (daily *main-page.storage*))))
    (if daily
        0
        (aif (delivery-price p)
             it
             (if (and (not (null g))
                      (delivery-price g))
                 (delivery-price g)
                 300))))
  )


;; (with-open-file (stream "/home/webadmin/Dropbox/htconf/test.csv")
;;     (do ((line (read-line stream nil)
;;                (read-line stream nil)))
;;         ((null line))
;;       ;; (print line)
;;       (let* ((words (split-sequence:split-sequence #\, line))
;;              (article (car words))
;;              (price (parse-integer (cadr words)))
;;              (siteprice (parse-integer (caddr words)))
;;              (prod (gethash article (storage *global-storage*))))
;;         (format t "~&~a: ~a ~a" article price siteprice)
;;         (if prod
;;             (setf (price prod) price)
;;             (setf (siteprice prod) siteprice))
;;       )))




;; (let ((res)
;;       (res1))
;;   (print "test")
;;   (maphash #'(lambda (k v)
;;                (if (and (equal (type-of v) 'product)
;;                         (new-classes.parent v)
;;                         (not (equal (type-of (new-classes.parent v)) 'group)))
;;                    (push v res)))
;;            (storage *global-storage*))
;;   (print (length res))
;;   (car res
;;   ;; (mapcar #'(lambda (v)
;;   ;;             (let ((key (key v)))
;;   ;;               (setf (key v) (format nil "~a" key))
;;   ;;               (storage.edit-object v)
;;   ;;               ;; (remhash key (storage *global-storage*)))
;;   ;;             ))
;;   ;;         res)
;;   ;; (print (length res1))
;;   )


;; (let ((res)
;;       (res1))
;;   (print "test")
;;   (maphash #'(lambda (k v)
;;                (if (equal (type-of (key v))
;;                           (type-of 132345))
;;                    (push v res)))
;;            (storage *global-storage*))
;;   (print (length res))
;;   ;; (mapcar #'(lambda (v)
;;   ;;             (let ((key (key v)))
;;   ;;               (setf (key v) (format nil "~a" key))
;;   ;;               (storage.edit-object v)
;;   ;;               ;; (remhash key (storage *global-storage*)))
;;   ;;             ))
;;   ;;         res)
;;   ;; (print (length res1))
;;   )


;; (let ((res)
;;       (res1))
;;   (print "test")
;;   (maphash #'(lambda (k v)
;;                (when (and (equal (type-of v) 'group)
;;                         (equal (fullfilter v) ""))
;;                  (setf (fullfilter v) nil)
;;                  (format t "~&~a:~a" k (fullfilter v))))
;;            (storage *global-storage*))
;;   (print (length res))
;;   ;; (mapcar #'(lambda (v)
;;   ;;             (let ((key (key v)))
;;   ;;               (setf (key v) (format nil "~a" key))
;;   ;;               (storage.edit-object v)
;;   ;;               ;; (remhash key (storage *global-storage*)))
;;   ;;             ))
;;   ;;         res)
;;   ;; (print (length res1))
;;   )


;; (mapcar #'(lambda (v) (setf (active v) nil))
;;  (storage.get-filtered-products (gethash "vinchester" (storage *global-storage*))))
;; (mapcar #'(lambda (v) (setf (active v) nil))
;;  (storage.get-filtered-products (gethash "vneshnie-zhostkie-diski" (storage *global-storage*))))

;; (create-report "seo/last-gateway-string.txt" #'show-last-history)
;; (time (create-report "xls/products.csv" #'write-products-report))
;; (create-report "seo/report-groups.csv" #'write-groups)
;; (create-report "seo/report-products.csv" #'write-products)
;; (create-report "seo/report-vendors.csv" #'write-vendors)
;; (create-report "seo/write-groups-active-product-num.csv" #'write-groups-active-product-num)


;; (progn
;;   (mapcar #'(lambda (v) (setf (groups v) nil))
;;           (groups *global-storage*))
;;   (mapcar #'(lambda (v)
;;               (mapcar #'(lambda (item)
;;                           (push v (groups item)))
;;                       (parents v)))
;;           (groups *global-storage*)))

;; (let ((rs))
;;   (maphash #'(lambda (k v)
;;                (declare (ignore k))
;;                (when (and (equal (type-of v) 'product)
;;                         (active v)
;;                         (= (siteprice v) 0))
;;                  (push v rs)
;;                  (setf (active v) nil)
;;                  (wlog (key v))))
;;            (storage *global-storage*))
;;   (length rs))


;; (let ((rs))
;;   (maphash #'(lambda (k v)
;;                (declare (ignore k))
;;                (when (and (equal (type-of v) 'article)
;;                           (not (null (title v))))
;;                  (push v rs)
;;                  (wlog (key v))))
;;            *storage-articles*)
;;   (length rs))

(defun report.delet-from-groups ()
  (let ((groups (storage.get-groups-list)))
    (mapcar #'(lambda (group)
                (setf (products group)
                      (remove-if-not #'(lambda (v) (let ((pr (gethash (key v) (storage *global-storage*))))
                                                     (and pr
                                                          (equal group (new-classes.parent pr)))))
                                     (products group))))
            groups)
    "done"))




(defun report.delete-doubles (products)
  (mapcar #'(lambda (v)
              ;; (wlog v)
              ;; (format t "rewrite ^/~a/?$ /~a permanent;~&" v v)
              (let ((pr (gethash (format nil "~a" v) (storage *global-storage*))))
                (when pr
                  (remhash (format nil "~a" v) (storage *global-storage*)))))
          products))



;; (mapcar #'(lambda (v)
;;             (if (not (equal 0 (hash-table-count (vendors-seo v))))
;;                 (maphash #'(lambda (k text)
;;                              (remhash k (vendors-seo v))
;;                              (setf (gethash (string-downcase (format nil "~a" k))
;;                                             (vendors-seo v)) text))
;;                          (vendors-seo v))))
;;         (groups *global-storage*))


;; (length (let ((rs))
;;          (maphash #'(lambda (k v)
;;                       (when (equal (type-of v) 'product)
;;                         (if (and (not (gethash (articul v) *xls.product-table*))
;;                                  (optgroups v))
;;                             (push v rs))))
;;                         (storage *global-storage*))
;;          rs))

;; (mapcar #'(lambda (v) (setf (ymlshow v) nil)) (groups (gethash "uslugi" (storage *global-storage*))))



(defun serials.all-prs ()
  (let ((rs))
    (maphash #'(lambda (k v)
                 (declare (ignore k))
                 (when (equal (type-of v) 'product)
                       (push v rs)))
             (storage *global-storage*))
         rs))


;; (mapcar #'(lambda (v)
;;             (let ((delta-p (price v))
;;                   (price (price v)))
;;               ((<= 0.2 (float (/ (abs (- siteprice-old siteprice 1)) siteprice-old))))))
;;         (serials.all-prs))


(defun create-sale-filter (group)
  (let* ((key (format nil "~a-~a" (key group) "sale"))
         (filter)
         (tmp-filter (car (remove-if-not #'(lambda (v) (equal (key v) key))
                                    (filters group)))))
    (if (not tmp-filter)
        (setf filter (make-instance 'filter))
        (setf filter tmp-filter))
    (setf (name filter) "Распродажа")
    (setf (func filter) #'groupd.is-groupd)
    (setf (key filter) key)
    (setf (parents filter) (list group))
    (when (not tmp-filter)
      (setf (gethash key (storage *global-storage*)) filter)
      (push filter (filters group)))
    filter
    ))

(defun report.add-products-to-group (product-list gr)
  ;; (wlog product-list)
  ;; (wlog gr)
  (mapcar #'(lambda (v)
             (let ((pr (gethash (format nil "~a" v) (storage *global-storage*))))
               (when pr
                  (setf (parents pr) (list gr))
                  (push pr (products gr))
                  (storage.edit-object pr)
                 (wlog pr))))
          product-list)
  "done")


(defun create-bestprice-filter (group)
  (let* ((key (format nil "~a-~a" (key group) "bestprice"))
         (filter)
         (tmp-filter (car (remove-if-not #'(lambda (v) (equal (key v) key))
                                    (filters group)))))
    (if (not tmp-filter)
        (setf filter (make-instance 'filter))
        (setf filter tmp-filter))
    (setf (name filter) "Горячий уик-энд скидок")
    (setf (func filter) #'(lambda (object) (> (delta-price object) 0)))
    (setf (key filter) key)
    (setf (parents filter) (list group))
    (when (not tmp-filter)
      (setf (gethash key (storage *global-storage*)) filter)
      (push filter (filters group)))
    filter
    ))


(defun create-man-sale-filter (group)
  (let* ((key (format nil "~a-~a" (key group) "23feb"))
         (filter)
         (tmp-filter (car (remove-if-not #'(lambda (v) (equal (key v) key))
                                    (filters group)))))
    (if (not tmp-filter)
        (setf filter (make-instance 'filter))
        (setf filter tmp-filter))
    (setf (name filter) "Подарки к 23 февраля")
    (setf (func filter) #'groupd.man.is-groupd)
    (setf (key filter) key)
    (setf (parents filter) (list group))
    (when (not tmp-filter)
      (setf (gethash key (storage *global-storage*)) filter)
      (push filter (filters group)))
    filter
    ))


(defun create-woman-sale-filter (group)
  (let* ((key (format nil "~a-~a" (key group) "8mart"))
         (filter)
         (tmp-filter (car (remove-if-not #'(lambda (v) (equal (key v) key))
                                    (filters group)))))
    (if (not tmp-filter)
        (setf filter (make-instance 'filter))
        (setf filter tmp-filter))
    (setf (name filter) "Подарки к 8 марта")
    (setf (func filter) #'groupd.woman.is-groupd)
    (setf (key filter) key)
    (setf (parents filter) (list group))
    (when (not tmp-filter)
      (setf (gethash key (storage *global-storage*)) filter)
      (push filter (filters group)))
    filter
    ))

(defun report.set-man-salefilter ()
  (mapcar #'(lambda (gr)
                (create-man-sale-filter gr))
          (storage.get-groups-list)))

(defun report.set-woman-salefilter ()
  (mapcar #'(lambda (gr)
                (create-woman-sale-filter gr))
          (storage.get-groups-list)))

(defun report.set-salefilter ()
  (mapcar #'(lambda (v)
              (let ((gr (gethash (format nil "~a" v) (storage *global-storage*))))
                (create-sale-filter gr)))
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





(defun report.test-get-dems (product)
  (let* ((articul (articul product))
         (path-art  (ppcre:regex-replace  "(\\d{1,3})(\\d{0,})"  (format nil "~a" articul)  "\\1/\\1\\2" ))
         (pic (car (get-pics articul)))
         (dest-pic-path (format nil "~a/~a/~a/~a"
                                *path-to-product-pics* "goods" path-art pic))
         (src-pic-path (format nil "~a/~a/~a/~a"
                               *path-to-product-pics* "big" path-art pic)))
    (if pic
        (multiple-value-bind (w h) (images-get-dimensions dest-pic-path)
          (when (< 300 h)
              (wlog (format nil "~a: ~aх~a" articul w h))
              (rename-convert src-pic-path dest-pic-path 225 300)
              )))))


(defun report.convert-name (input-string)
  (string-trim (list #\Space #\Tab #\Newline)
               (format nil "~:(~a~)" input-string)))




(defun report.transfer-group (g1 g2)
  (when (storage.main-parent g1)
    (setf (groups (storage.main-parent g1)) (remove-if #'(lambda (v) (equal v g1)) (groups (storage.main-parent g1))))
    (wlog "(setf (groups (storage.")
    (setf (parents g1) nil)
    (wlog "(setf (parents g1) nil))"))
  (when g2
    (push g1 (groups g2))
    (wlog "    (push  g1 (groups g2)")
    (setf (parents g1) (list g2))
    (wlog "    (setf (parents g1) (list g2))"))
  )

;; (report.transfer-group
;; (gethash "otrazhateli" (storage *global-storage*))
;; (gethash "studiynoe-oborudovanie" (storage *global-storage*)))



(defun report.get-groups (group)
  (if (null (groups group))
      (format t "~&(equal key \"~a\")" (key group))
      (mapcar #'report.get-groups (groups group))))


(defun report.do-seo-reports ()
   (let ((name (format nil "reports/seo-report-groups-~a.csv" (time.encode.backup-filename))))
     (create-report name #'write-groups)
    (wlog "Groups SEO report"))
   (let ((name (format nil "reports/seo-report-vendors-~a.csv" (time.encode.backup-filename))))
     (create-report name #'write-vendors)
    (wlog "Vendors SEO report"))
   (let ((name (format nil "reports/seo-report-products-~a.csv" (time.encode.backup-filename))))
     (create-report name #'write-products)
    (wlog "Products SEO report")))



(defun report.write-alias (&optional (stream *standard-output*))
    (format stream "имя группы; наличие алиасов; группа опций ; опция; имя алиаса; ед. измерения;")
    (mapcar #'(lambda (gr)
                (if (null (catalog-keyoptions gr))
                    (format stream "~&~a; нет;" (name gr))
                    (mapcar #'(lambda (alias)
                                (let ((alias-temp (remove-if #'null
                                                             (mapcar #'(lambda (v)
                                                                         (if (not (equal (type-of v)
                                                                                         'keyword))
                                                                         (stripper v)))
                                                                     alias))))
                                  (format stream "~&\"~a\";~a;~{\" ~a \";~}"
                                          (stripper (name gr))
                                          (if alias-temp "есть" "нет")
                                          alias-temp)))
                            (catalog-keyoptions gr))))
            (storage.get-groups-list)))

(defun report.write-keyoptions (&optional (stream *standard-output*))
    (format stream "имя группы; наличие ключевых опций; группа опций ; опция;")
    (mapcar #'(lambda (gr)
                (if (null (keyoptions gr))
                    (format stream "~&~a; нет;" (name gr))
                    (mapcar #'(lambda (alias)
                                (let ((alias-temp (remove-if #'null
                                                             (mapcar #'(lambda (v)
                                                                         (if (not (equal (type-of v)
                                                                                         'keyword))
                                                                             (stripper v)))
                                                                     alias))))
                                  (format stream "~&\"~a\";~a;~{\" ~a \";~}"
                                          (stripper (name gr))
                                          (if alias-temp "есть" "нет")
                                          alias-temp)))
                            (keyoptions gr))))
            (storage.get-groups-list)))



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
                               *path-to-product-pics* "big" path-art pic)))
                  ;; (multiple-value-bind (w h) (images-get-dimensions src-pic-path)
                  (with-open-file
                      (stream-file src-pic-path)
                    (when (= (file-length stream-file) 0)
                      (setf is-need-reconvert t)
                      (format stream "~&~a;~a" articul (file-length stream-file))))
                  ;; )
                  ))
            (get-pics articul))
    is-need-reconvert
    ))



(defun report.write-pictures (&optional (stream *standard-output*))
  (let ((num 0))
    (format stream "артикул;имя;файл;ширина;высота;размер;")
    (mapcar #'(lambda (gr)
                (mapcar #'(lambda (product)
                            (let* ((articul (articul product))
                                   (path-art  (ppcre:regex-replace  "(\\d{1,3})(\\d{0,})"  (format nil "~a" articul)  "\\1/\\1\\2" )))
                              (mapcar #'(lambda (pic)
                                          (let ((src-pic-path
                                                 (format nil "~a/~a/~a/~a"
                                                         *path-to-product-pics* "big" path-art pic)))
                                            ;; (multiple-value-bind (w h) (images-get-dimensions src-pic-path)
                                            (with-open-file
                                                (stream-file src-pic-path)

                                              (when (= (file-length stream-file) 0)
                                                (incf num)
                                                (format stream "~&~a;~a" articul (file-length stream-file))
                                                )
                                              )
                                            ;; )
                                            ))
                                      (get-pics articul))))
                        (products gr)))
            (storage.get-groups-list))
    num))


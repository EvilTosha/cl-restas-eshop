;;;; yml.lisp

(in-package #:eshop)

;; Группы представляют собой лес для YML нам нужны не только сами
;; группы маркированные для выгрузки но и их родители
;; На самом деле нам нужно минимальные остовные деревья,
;; но будем выгружать полные деревья исключая только листья, если это нужно

(defparameter *yml-group-ids* (make-hash-table :test #'equal))

(defun yml-groups ()
  "Строим *yml-group-ids*"
  (let ((current-id 1))
    (clrhash *yml-group-ids*)
    (process-storage
     #'(lambda (gr)
         ;; Если группа имеет дочерние группы ИЛИ yml-show == true
         (when (or (groups gr) (ymlshow gr))
           ;; Кладем ее в *yml-group-ids* и увеличиваем current-id
           (setf (gethash (key gr) *yml-group-ids*) current-id )
           (incf current-id)))
     'group)
    *yml-group-ids*))

(defun yml.yml-show-p (product)
  (declare (product product))
  (and (parent product)
       (ymlshow (parent product))
       (or (active product) (yml.available-for-order-p product))
       (price product)
       (plusp (price product))
       ;;для селективного исключения товаров по значению специальной опции
       (let ((yml-show (get-option product "Secret" "YML")))
         (not (and yml-show
                   (string= "No" (stripper yml-show)))))))

(defun yml.get-product-delivery-price (product)
  (let ((parent (parent product)))
    (if (delivery-price product)
        (delivery-price product)
        (if (and parent (delivery-price parent))
            (delivery-price parent)
            300))))

(defun yml.is-daily-product (product)
  (loop
     :for v :being :the hash-values :in (daily *main-page.storage*)
     :thereis (and (equal (key v) (key product))
                   (< (date-start v) (get-universal-time) (date-finish v)))))

(defun yml.get-delivery-price (cart)
  (let ((result 300)
        (min-price 500)
        (max-price 0))
    (mapcar #'(lambda (v)
                (let* ((product (getobj (getf v :articul) 'product))
                       (d (yml.get-product-delivery-price1 product)))
                  (if (> d max-price)
                      (setf max-price d))
                  (if (< d min-price)
                      (setf min-price d))))
            cart)
    (if (= max-price 500)
        (setf result 500)
        (setf result min-price))
    result))


(defun yml.get-product-delivery-price1 (product)
  (let ((parent (if product (parent product)))
        (key)
        (result 300))
    (when parent
      (setf key (key parent))
      (if (or (equal key "krupnaya-bitivaya-tehnika")
              (equal key "vstraivaemye-rabochie-poverhnosti")
              (equal key "vstraivaemye-rabochie-komplekti")
              (equal key "vityajki")
              (equal key "stiralnie-mashiny")
              (equal key "posudomoechnie-mashiny")
              (equal key "plity")
              (equal key "holodilniki-i-morozilniki")
              (equal key "duhovki")
							(equal key "kondicioneri")
              (let ((diagonal (get-option product "Экран" "Диагональ экрана, дюйм")))
                (if (equal diagonal "")
                    (setf diagonal nil))
                (setf diagonal (ceiling (arnesi:parse-float diagonal)))
                (> diagonal 32)))
          (setf result 500)
          (if (or
               ;; (equal key "netbuki")
               ;; (equal key "noutbuki")
               ;; (equal key "planshetnie-komputery")
               ;; (equal key "komputery")
               ;; (and (equal (vendor product) "Brother")
               ;;      (or (equal key "printery")
               ;;          (equal key "mfu")
               ;;          (equal key "faxes")))
               (yml.is-daily-product product))
              (setf result 0)
              (if (or
                   ;; (equal key "cifrovye-fotoapparaty")
                   ;; (equal key "mobilephones")
                   ;; (equal key "planshetnie-komputery")
                   ;; (equal key "ustroistva-dlya-chtenia-electronnyh-knig")
                   ;; (equal key "mp3-pleery")
                   ;; (equal key "fotoramki")
                   ;; (equal key "gps-navigatory")
                   ;; (equal key "eholoti")
                   ;; (equal key "marshrutizatory-i-tochki-dostupa")
                   ;; (equal key "vneshnie-zhostkie-diski")
                   ;; (equal key "myshki")
                   ;; (equal key "klaviatury")
                   ;; (equal key "joystiki-ruli-gamepady")
                   ;; (equal key "web-camery")
                   ;; (equal key "graficheskie-planshety")
                   ;; (equal key "cartridge-dlya-printerov")
                   )
                  (setf result 200)))))
    result))



(defun yml-page ()
  (setf (hunchentoot:content-type*) "application/xml; charset=utf-8")
  (soy.yml:xml
   (list :datetime (time.get-date-time)
         :marketname "ЦиFры 320-8080"
         :marketcompany "ЦиFры 320-8080"
         :marketurl "http://www.320-8080.ru/"
         :categoryes
         (loop
            :for key
            :being :the hash-key
            :in (yml-groups)
            :when (getobj key 'group)
            :collect (let ((obj (getobj key 'group)))
                       (list :id (yml-id obj)
                             :name (name obj)
                             :parent (if (null (parent obj))
                                         0 ; если это вершина дерева
                                         (yml-id (parent obj))))))
         :offers (format nil "~{~a~}"
                         (collect-storage
                          'product
                          ;;продукт должен находиться в группе маркированной как ymlshow
                          ;;быть активным и иметь не нулевую цену
                          :when-fn #'yml.yml-show-p
                          :func #'(lambda (product)
                                    (soy.yml:offer (list :articul (articul product)
                                                         :available (active product) ; если не active, то прошел available-for-order
                                                         :deliveryprice (yml.get-product-delivery-price1 product)
                                                         :price (siteprice product)
                                                         :category (yml-id (parent product))
                                                         :picture (let ((pics (get-pics
                                                                               (key product))))
                                                                    (when pics
                                                                      (encode-uri (car pics))))
                                                         :name (let ((yml-name (get-option product "Secret" "Yandex")))
                                                                 (if (or (null yml-name)
                                                                         (string= ""
                                                                                  (stripper yml-name))
                                                                         (string= "No"
                                                                                  (stripper yml-name)))
                                                                     (name-seo product)
                                                                     yml-name))
                                                         :description nil))))))))


(defun yml-page-for-parser ()
  (setf (hunchentoot:content-type*) "application/xml; charset=utf-8")
  (soy.yml:xml
   (list :datetime (time.get-date-time)
         :marketname "ЦиFры 320-8080"
         :marketcompany "ЦиFры 320-8080"
         :marketurl "http://www.320-8080.ru/"
         :categoryes
         (loop
            :for key
            :being :the hash-key
            :in (yml-groups)
            :when (getobj key 'group)
            :collect (let ((obj (getobj key 'group)))
                       (list :id (yml-id obj)
                             :name (name obj)
                             :parent (if (null (parent obj))
                                         0 ; если это вершина дерева
                                         (yml-id (parent obj))))))
         :offers (format nil "~{~a~}"
                         (collect-storage
                          'product
                          ;;продукт должен находиться в группе маркированной как ymlshow
                          ;;быть активным и иметь не нулевую цену
                          :when-fn #'yml.yml-show-p
                          :func #'(lambda (product)
                                    (soy.yml:offer (list :articul (articul product)
                                                         :price (siteprice product)
                                                         :category (gethash
                                                                    (key (parent product))
                                                                    *yml-group-ids*)
                                                         :picture (let ((pics (get-pics
                                                                               (articul product))))
                                                                    (when pics
                                                                      (encode-uri (car pics))))
                                                         :name (let ((yml-name (get-option
                                                                                product "Secret" "Yandex"))
                                                                     (parser-name (get-option
                                                                                   product "Secret" "Parser")))
                                                                 (if (valid-string-p parser-name)
                                                                     parser-name
                                                                     (if (or (null yml-name)
                                                                             (string= ""
                                                                                      (stripper yml-name))
                                                                             (string= "No"
                                                                                      (stripper yml-name)))
                                                                         (name product)
                                                                         yml-name)))
                                                         :description nil))))))))


(defun make-yml-categoryes()
  (loop
     :for key
     :being :the hash-key
     :using (hash-value id)
     :in (yml-groups)
     :when (getobj key 'group)
     :collect (let ((obj (getobj key 'group)))
                (list :id (yml-id obj)
                      :name (name obj)
                      :parent (if (null (parent obj))
                                  0 ; если это вершина дерева
                                  (yml-id (parent obj)))))))

(defun make-yml-offers()
  (collect-storage
   'product
   ;;продукт должен находиться в группе маркированной как ymlshow
   ;;быть активным и иметь не нулевую цену
   :when-fn #'yml.yml-show-p
   :func #'(lambda (product)
             (soy.yml:offer (list :articul (articul product)
                                  :deliveryprice (yml.get-product-delivery-price1 product)
                                  :price (siteprice product)
                                  :category (gethash
                                             (key (parent product))
                                             *yml-group-ids*)
                                  :picture  (let ((pics (get-pics
                                                         (articul product))))
                                              (when pics
                                                (encode-uri (car pics))))
                                  :name   (let ((yml-name (get-option product "Secret" "Yandex")))
                                            (if (or (null yml-name)
                                                    (string= ""
                                                             (stripper yml-name))
                                                    (string= "No"
                                                             (stripper yml-name)))
                                                (name-seo product)
                                                yml-name))
                                  :description nil)))))


(defun make-yml-data()
  (soy.yml:xml
   (list :datetime (time.get-date-time)
         :marketname "ЦиFры 320-8080"
         :marketcompany "ЦиFры 320-8080"
         :marketurl "http://www.320-8080.ru/"
         :categoryes (make-yml-categoryes)
         :offers (format nil "~{~a~}" (make-yml-offers)))))


(defun create-yml-file ()
  (let ((filename (merge-pathnames "yml.xml" (config.get-option "CRITICAL" "path-to-conf"))))
    (with-open-file
        (stream filename :direction :output :if-exists :supersede)
      (format stream "~a" (make-yml-data)))))

(defun yml.test-groups (group)
  (mapcar #'(lambda (v)
              (if (groups v)
                  (yml.test-groups v)
                  (format t "~& (equal key \"~a\")" (key v))))
          (groups group)))


(defun yml.get-next-yml-id ()
  "Generate uniq group id for yanedx market. Max current id +1."
  (if (not (get-storage 'group))
      0
      (let ((max 0))
        (process-storage #'(lambda (gr)
                             (let ((id (yml-id gr)))
                               (when (and id (< max id))
                                 (setf max id))))
                         'group)
        (1+ max))))

(defun yml.available-for-order-p (product)
  (declare (product product))
  ;; life-time is given in days
  (let ((parent (parent product)))
    (when (and parent (life-time parent) (plusp (life-time parent)))
      (< (get-universal-time) (+ (date-modified product)
                                 (* 60 60 24 (life-time parent)))))))


(defun yml.count-products-for-order (&optional group)
  "Count number of products, which are not active, but available for order by calling"
  (declare ((or group null) group))
  (let ((products (if group
                      (storage.get-recursive-products group (complement #'active))
                      (collect-storage 'product :when-fn (complement #'active)))))
    (count-if #'yml.available-for-order-p products)))

(defun yml.pretty-count-products-for-order ()
  "Count number of products, which are not active, with pretty print for all groups"
  (format nil "~{~A~%~}"
          (mapcar #'(lambda (gr)
                      (format nil "Group: ~A, products for order: ~D"
                              (key gr)
                              (yml.count-products-for-order gr)))
                  (sort
                   (collect-storage 'group
                                    :when-fn
                                    #'(lambda (gr)
                                        (plusp (yml.count-products-for-order gr))))
                   #'< :key #'yml.count-products-for-order))))

(defun yml.get-list-for-order (group)
  (declare (group group))
  (remove-if-not #'yml.available-for-order-p
                 (storage.get-recursive-products group (complement #'active))))


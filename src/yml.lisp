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
  (and (typep product 'product)
       (class-core.parent product)
       (ymlshow (class-core.parent product))
       (active product)
       (price product)
       (plusp (price product))
       ;;для селективного исключения товаров по значению специальной опции
       (let ((yml-show))
         (with-option1 product "Secret" "YML"
                       (setf yml-show (getf option :value)))
         (not (and yml-show
                   (string= "No" (stripper yml-show)))))))

(defun yml.get-product-delivery-price (product)
  (let ((parent (class-core.parent product)))
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
  (let ((parent (if product (class-core.parent product)))
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
              (let ((diagonal))
                (with-option1 product "Экран" "Диагональ экрана, дюйм"
                              (setf diagonal (getf option :value)))
                (if (equal diagonal "")
                    (setf diagonal nil))
                (setf diagonal (ceiling (arnesi:parse-float diagonal)))
                (> diagonal 32)))
          (setf result 500)
          (if (or
               ;; (and (equal (vendor product) "Brother")
               ;;      (or (equal key "printery")
               ;;          (equal key "mfu")
               ;;          (equal key "faxes")))
               (yml.is-daily-product product))
              (setf result 0)
              (if (or
                   ;; (equal key "mobilephones")
                   ;; (equal key "planshetnie-komputery")
                   ;; (equal key "cifrovye-fotoapparaty")
                   ;; (equal key "kuhonnie-vesy")
                   ;; (equal key "kofevarki")
                   ;; (equal key "kofemolki")
                   ;; (equal key "aksessuary-dlya-bytovoi-tehniki")
                   ;; (equal key "tostery")
                   ;; (equal key "friturnicy")
                   ;; (equal key "buterbrodnicy")
                   ;; (equal key "parovarki")
                   ;; (equal key "electrovarki")
                   ;; (equal key "aerogrili")
                   ;; (equal key "mikrovolnovye-pechi")
                   ;; (equal key "hlebopechki")
                   ;; (equal key "blinnicy")
                   ;; (equal key "myasorubki")
                   ;; (equal key "kuhonnye-kombainy")
                   ;; (equal key "izmelchiteli")
                   ;; (equal key "elektrochainiki-i-termopoty")
                   ;; (equal key "sokovyzhimalki")
                   ;; (equal key "mixery")
                   ;; (equal key "blendery")
                   ;; (equal key "vesi-napolnie")
                   ;; (equal key "trimmery")
                   ;; (equal key "shipci")
                   ;; (equal key "gigiena-i-zdorovie")
                   ;; (equal key "feny")
                   ;; (equal key "epilyatory")
                   ;; (equal key "britvy")
                   )
                  (setf result 100)))))
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
                             :parent (if (null (class-core.parent obj))
                                         0 ; если это вершина дерева
                                         (yml-id (class-core.parent obj))))))
         :offers (format nil "~{~a~}"
                         (process-and-collect-storage
                          'product
                          ;;продукт должен находиться в группе маркированной как ymlshow
                          ;;быть активным и иметь не нулевую цену
                          :when-func #'yml.yml-show-p
                          :func #'(lambda (product)
                                    (soy.yml:offer (list :articul (articul product)
                                                         :available (servo.available-for-order-p product)
                                                         :deliveryprice (yml.get-product-delivery-price1 product)
                                                         :price (siteprice product)
                                                         :category (yml-id (class-core.parent product))
                                                         :picture (let ((pics (get-pics
                                                                               (articul product))))
                                                                    (when pics
                                                                      (encode-uri (car pics))))
                                                         :name (let ((yml-name))
                                                                 (with-option1 product "Secret" "Yandex"
                                                                               (setf yml-name (getf option :value)))
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
                             :parent (if (null (class-core.parent obj))
                                         0 ; если это вершина дерева
                                         (yml-id (class-core.parent obj))))))
         :offers (format nil "~{~a~}"
                         (process-and-collect-storage
                          'product
                          ;;продукт должен находиться в группе маркированной как ymlshow
                          ;;быть активным и иметь не нулевую цену
                          :when-func #'yml.yml-show-p
                          :func #'(lambda (product)
                                    (soy.yml:offer (list :articul (articul product)
                                                         :price (siteprice product)
                                                         :category (gethash
                                                                    (key (class-core.parent product))
                                                                    *yml-group-ids*)
                                                         :picture (let ((pics (get-pics
                                                                               (articul product))))
                                                                    (when pics
                                                                      (encode-uri (car pics))))
                                                         :name (let ((yml-name)
                                                                     (parser-name))
                                                                 (with-option1 product "Secret" "Yandex"
                                                                               (setf yml-name (getf option :value)))
                                                                 (with-option1 product "Secret" "Parser"
                                                                               (setf parser-name (getf option :value)))
                                                                 (if (or (null parser-name)
                                                                         (string= "" parser-name))
                                                                     (if (or (null yml-name)
                                                                             (string= ""
                                                                                      (stripper yml-name))
                                                                             (string= "No"
                                                                                      (stripper yml-name)))
                                                                         (name product)
                                                                         yml-name)
                                                                     parser-name))
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
                      :parent (if (null (class-core.parent obj))
                                  0 ; если это вершина дерева
                                  (yml-id (class-core.parent obj)))))))

(defun make-yml-offers()
  (process-and-collect-storage
   'product
   ;;продукт должен находиться в группе маркированной как ymlshow
   ;;быть активным и иметь не нулевую цену
   :when-func #'yml.yml-show-p
   :func #'(lambda (product)
             (soy.yml:offer (list :articul (articul product)
                                  :deliveryprice (yml.get-product-delivery-price1 product)
                                  :price (siteprice product)
                                  :category (gethash
                                             (key (class-core.parent product))
                                             *yml-group-ids*)
                                  :picture  (let ((pics (get-pics
                                                         (articul product))))
                                              (when pics
                                                (encode-uri (car pics))))
                                  :name   (let ((yml-name))
                                            (with-option1 product "Secret" "Yandex"
                                                          (setf yml-name (getf option :value)))
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

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
    (maphash #'(lambda(k g)
                 (when (equal (type-of g) 'group)
                   ;; Если группа имеет дочерние группы ИЛИ yml-show == true
                   (when (or
                          (not (null (groups g)))
                          (ymlshow g))
                     ;; Кладем ее в *yml-group-ids* и увеличиваем current-id
                     (setf (gethash k *yml-group-ids*) current-id )
                     (incf current-id))))
             (storage *global-storage*))
    *yml-group-ids*))

(defun yml.is-yml-show (product)
  (and (equal 'product (type-of product))
       (not (null (new-classes.parent product)))
       (ymlshow (new-classes.parent product))
       (active product)
       (not (null (price product)))
       (> (price product) 0)
       ;;для селективного исключения товаров по значению специальной опции
       (let ((yml-show))
         (with-option1 product "Secret" "YML"
                       (setf yml-show (getf option :value)))
         (if (and (not (null yml-show))
                  (string= "No"
                           (stripper yml-show)))
             nil
             t))))

(defun yml.get-product-delivery-price (product)
  (let ((parent (new-classes.parent product)))
    (if (delivery-price product)
        (delivery-price product)
        (if (and parent (delivery-price parent))
            (delivery-price parent)
            300))))

(defun yml.is-daily-product (product)
  (let ((result nil))
    (maphash #'(lambda (k v)
                 (declare (ignore k))
                 (when (and (equal (key v) (key product))
                            (< (date-start v) (get-universal-time) (date-finish v)))
                   (setf result t)))
             (daily *main-page.storage*))
    result))

(defun yml.get-delivery-price (cart)
  (let ((result 300)
        (min-price 500)
        (max-price 0))
    (mapcar #'(lambda (v)
                (let* ((product (gethash (getf v :articul) (storage *global-storage*)))
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
  (let ((parent (if product (new-classes.parent product)))
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
              (let ((diagonal))
								(with-option1 product "Экран" "Диагональ экрана, дюйм"
															(setf diagonal (getf option :value)))
								(if (equal diagonal "")
										(setf diagonal nil))
								(setf diagonal (ceiling (arnesi:parse-float diagonal)))
								(> diagonal 32)))
          (setf result 500)
          (if (or
							 (and (equal (vendor product) "Brother")
										(or (equal key "printery")
												(equal key "mfu")
												(equal key "faxes")))
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
  (yml:xml (list :datetime (time.get-date-time)
                 :marketname "ЦиFры 320-8080"
                 :marketcompany "ЦиFры 320-8080"
                 :marketurl "http://www.320-8080.ru/"
                 :categoryes
                 (loop
                    :for key
                    :being :the hash-key
                    :using (hash-value id)
                    :in (yml-groups)
                    :when (equal 'group (type-of (gethash key (storage *global-storage*))))
                    :collect (list :id id
                                   :name (name (gethash key (storage *global-storage*)))
                                   :parent (if (null (new-classes.parent (gethash key (storage *global-storage*))))
                                               0 ;; если это вершина дерева
                                               (let* ((parent (new-classes.parent (gethash key (storage *global-storage*))))
                                                      (parent-key (key parent))
                                                      (num-key (gethash parent-key *yml-group-ids*)))
                                                 num-key))))
                 :offers (format nil "~{~a~}"
                                 (loop
                                    :for product
                                    :being :the hash-values
                                    :in (storage *global-storage*)
                                    ;;продукт должен находиться в группе маркированной как ymlshow
                                    ;;быть активным и иметь не нулевую цену
                                    :when (and (equal 'product (type-of product))
                                               (not (null (new-classes.parent product)))
                                               (ymlshow (new-classes.parent product))
                                               (active product)
                                               (not (null (price product)))
                                               (> (price product) 0)
                                               ;;для селективного исключения товаров по значению специальной опции
                                               (let ((yml-show))
                                                 (with-option1 product "Secret" "YML"
																															 (setf yml-show (getf option :value)))
                                                 (if (and (not (null yml-show))
                                                          (string= "No"
                                                                   (stripper yml-show)))
                                                     nil
                                                     t)))
                                    :collect (yml:offer (list :articul (articul product)
                                                              :available (servo.available-for-order-p product)
                                                              :deliveryprice (yml.get-product-delivery-price1 product)
                                                              :price (siteprice product)
                                                              :category (gethash
                                                                         (key (new-classes.parent product))
                                                                         *yml-group-ids*)
                                                              :picture  (let ((pics (get-pics
                                                                                     (articul product))))
                                                                          (if (null pics)
                                                                              nil
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
                                                              :description nil)))))))


(defun yml-page-for-parser ()
  (setf (hunchentoot:content-type*) "application/xml; charset=utf-8")
  (yml:xml (list :datetime (time.get-date-time)
                 :marketname "ЦиFры 320-8080"
                 :marketcompany "ЦиFры 320-8080"
                 :marketurl "http://www.320-8080.ru/"
                 :categoryes
                 (loop
                    :for key
                    :being :the hash-key
                    :using (hash-value id)
                    :in (yml-groups)
                    :when (equal 'group (type-of (gethash key (storage *global-storage*))))
                    :collect (list :id id
                                   :name (name (gethash key (storage *global-storage*)))
                                   :parent (if (null (new-classes.parent (gethash key (storage *global-storage*))))
                                               0 ;; если это вершина дерева
                                               (let* ((parent (new-classes.parent (gethash key (storage *global-storage*))))
                                                      (parent-key (key parent))
                                                      (num-key (gethash parent-key *yml-group-ids*)))
                                                 num-key))))
                 :offers (format nil "~{~a~}"
                                 (loop
                                    :for product
                                    :being :the hash-values
                                    :in (storage *global-storage*)
                                    ;;продукт должен находиться в группе маркированной как ymlshow
                                    ;;быть активным и иметь не нулевую цену
                                    :when (and (equal 'product (type-of product))
                                               (not (null (new-classes.parent product)))
                                               (ymlshow (new-classes.parent product))
                                               (active product)
                                               (not (null (price product)))
                                               (> (price product) 0)
                                               ;;для селективного исключения товаров по значению специальной опции
                                               (let ((yml-show))
                                                 (with-option1 product "Secret" "YML"
																															 (setf yml-show (getf option :value)))
                                                 (if (and (not (null yml-show))
                                                          (string= "No"
                                                                   (stripper yml-show)))
                                                     nil
                                                     t)))
                                    :collect (yml:offer (list :articul (articul product)
                                                              :price (siteprice product)
                                                              :category (gethash
                                                                         (key (new-classes.parent product))
                                                                         *yml-group-ids*)
                                                              :picture  (let ((pics (get-pics
                                                                                     (articul product))))
                                                                          (if (null pics)
                                                                              nil
                                                                              (encode-uri (car pics))))
                                                              :name   (let ((yml-name)
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
                                                              :description nil)))))))


(defun yml-name-test (product)
  (let ((yml-name))
    (with-option1 product "Secret" "Yandex"
									(setf yml-name (getf option :value)))
    (if (or (null yml-name)
            (string= ""
                     (stripper yml-name))
            (string= "No"
                     (stripper yml-name)))
        (name product)
        yml-name)))

(defun yml-show-test (product)
  (let ((yml-show))
    (with-option1 product "Secret" "UML"
									(setf yml-show (getf option :value)))
    (if (and (not (null yml-show))
             (string= "No"
                      (stripper yml-show)))
        nil
        t)))

(defun make-yml-categoryes()
  (loop
     :for key
     :being :the hash-key
     :using (hash-value id)
     :in (yml-groups)
     :when (equal 'group (type-of (gethash key (storage *global-storage*))))
     :collect (list :id id
                    :name (name (gethash key (storage *global-storage*)))
                    :parent (if (null (new-classes.parent (gethash key (storage *global-storage*))))
                                0 ;; если это вершина дерева
                                (let* ((parent (new-classes.parent (gethash key (storage *global-storage*))))
                                       (parent-key (key parent))
                                       (num-key (gethash parent-key *yml-group-ids*)))
                                  num-key)))))

(defun make-yml-offers()
	(loop
     :for product
     :being :the hash-values
     :in (storage *global-storage*)
     ;;продукт должен находиться в группе маркированной как ymlshow
     ;;быть активным и иметь не нулевую цену
     :when (and (equal 'product (type-of product))
                (not (null (new-classes.parent product)))
                (ymlshow (new-classes.parent product))
                (active product)
                (not (null (price product)))
                (> (price product) 0)
                ;;для селективного исключения товаров по значению специальной опции
                (let ((yml-show))
                  (with-option1 product "Secret" "UML"
																(setf yml-show (getf option :value)))
                  (if (and (not (null yml-show))
                           (string= "No"
                                    (stripper yml-show)))
                      nil
                      t)))
     :collect
     (yml:offer (list :articul (articul product)
                      :deliveryprice (yml.get-product-delivery-price1 product)
                      :price (siteprice product)
                      :category (gethash
                                 (key (new-classes.parent product))
                                 *yml-group-ids*)
                      :picture  (let ((pics (get-pics
                                             (articul product))))
                                  (if (null pics)
                                      nil
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
                      :description nil))))


(defun make-yml-data()
  (yml:xml
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

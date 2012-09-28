;;; newcart.lisp

(in-package #:eshop)

;;обновление страницы
(defun newcart-update ()
  (newcart-compile-templates))

;;шаблоны
(defun newcart-compile-templates ()
  (servo.compile-soy "index.soy"
                     "newcart.soy"))

;; возвращает список отображений продуктов, количество, суммарную цену заказа
(defun newcart-cart-products (alist)
  (let ((counter 0)
        (sum-counter 0)
        (sum 0)
        (res-list)
        (bonuscount 0))
    (setf res-list (mapcar #'(lambda (item)
                               (let ((articul (cdr (assoc :id  item)))
                                     (cnt     (cdr (assoc :count item)))
                                     (product)
                                     (price 0))
                                 (when (and articul cnt)
                                   (setf cnt (parse-integer (format nil "~a" cnt) :junk-allowed t))
                                   (setf product (getobj articul 'product))
                                   (when (and product
                                              (plusp cnt))
                                     (incf counter)
                                     (setf price (if (zerop (siteprice product))
                                                     (price product)
                                                     (siteprice product)))
                                     (setf sum (+ sum (* cnt price)))
                                     (setf bonuscount (+ bonuscount (* cnt (bonuscount product) 10)))
                                     (setf sum-counter (+ sum-counter cnt))
                                     (list :numpos counter
                                           :count cnt
                                           :name (name-seo product)
                                           :price (if (zerop price)
                                                      ""
                                                      price)
                                           :group (aif (car (parents product))
                                                       (name it)
                                                       "Без группы")
                                           ;;данные для корзины
                                           :siteprice (siteprice product)
                                           :articul (if (= 5 (length articul))
                                                        (format nil "0~a" articul)
                                                        articul)
                                           :url (format nil "/~a" articul)
                                           :firstpic (car (get-pics articul))
                                           ;;для sendmail
                                           :cnt cnt
                                           :sum (* cnt price))))))
                           alist))
    (values-list (list res-list sum-counter sum bonuscount))))

(defun newcart-tovar (n)
  (let ((k n))
    (if (< 20 n)
        (setf k (mod n 10)))
    (if (zerop k)
        "товаров"
        (if (= 1 k)
            "товар"
            (if (> 5 k)
                "товара"
                "товаров")))))

(defun newcart-yandex-cookie ()
  (let* ((cookie (hunchentoot:cookie-in "user-nc"))
         (cookie-data (when cookie
                        (servo.alist-to-plist (decode-json-from-string cookie))))
         (post-data (servo.alist-to-plist (hunchentoot:post-parameters hunchentoot:*request*)))
         (street (getf post-data :street))
         (building (getf post-data :building))
         (suite (getf post-data :suite))
         (entrance (getf post-data :entrance))
         (flat (getf post-data :flat))
         (floor (getf post-data :floor))
         (firstname (getf post-data :firstname))
         (fathersname (getf post-data :fathersname))
         (family (getf post-data :lastname))
         (phone (getf post-data :phone))
         (email (getf post-data :email))
         (intercom (getf post-data :intercom))
         (phone-extra (getf post-data :phone-extra))
         (comment (getf post-data :comment))
         name
         addr
         courier-comment)
    (setf name (report.convert-name
                (format nil "~a ~a" firstname fathersname)))
    (setf addr (format nil "~{~a~^, ~}"
                       (loop
                          :for (x y)
                          :in (list (list street "")
                                    (list building "")
                                    (list suite "к.")
                                    (list entrance "подъезд")
                                    (list floor "этаж")
                                    (list flat "кв."))
                          :when (and x (string/= x ""))
                          :collect (format nil "~a ~a" y x))))
    (setf courier-comment
          (format nil "~{~a~^; ~}"
                  (loop
                     :for (x y)
                     :in (list (list intercom "Домофон:")
                               (list phone-extra "Доп. телефон:")
                               (list comment "Комментарий:"))
                     :when (and x (string/= x ""))
                     :collect (format nil "~a ~a" y x))))
    (loop
       :for (x y)
       :in (list (list :addr addr)
                 (list :name name)
                 (list :family family)
                 (list :phone phone)
                 (list :email email)
                 (list :courier_comment courier-comment))
       :when (and y (string/= y ""))
       :do (setf (getf cookie-data x) y))
    (setf (getf cookie-data "delivery-type") "express")
    (hunchentoot:set-cookie "user-nc"
                            :value (format nil "{~{\"~(~a~)\":\"~a\"~^,~}}" cookie-data))))


;;отображение страницы
(defun newcart-show (&optional (request-str ""))
  (declare (ignore request-str))
  (when (assoc "operation_id" (hunchentoot:post-parameters hunchentoot:*request*)  :test #'string=)
    (newcart-yandex-cookie))
  (let ((cart-cookie (hunchentoot:cookie-in "cart"))
        (cart)
        (products)
        (count 0)
        (pricesum 0)
        (bonuscount nil))
    (when (not (null cart-cookie))
      (setf cart (json:decode-json-from-string cart-cookie))
      (multiple-value-bind (lst cnt sm bc) (newcart-cart-products cart)
        (setf products (remove-if #'null lst))
        (setf count cnt)
        (setf pricesum sm)
        (if (and bc
                 (not (equal bc 0)))
            (setf bonuscount bc))))
    (if (and (not (null products))
             (< 0 pricesum)
             (< 0 count))
        (progn
          (soy.newcart:fullpage (list :head (soy.newcart:head)
                                      :leftcells (soy.newcart:leftcells
                                                  (list :bonuscount bonuscount
                                                        :bonusname (if bonuscount
                                                                       (nth (skls.get-count-skls bonuscount)
                                                                            (list "бонус" "бонуса" "бонусов")))))
                                      :rightcells (soy.newcart:rightcells
                                                   (list :notfinished "true"
                                                         :deliverysum pricesum
                                                         :productscount count
                                                         :tovar (newcart-tovar count)
                                                         :products (mapcar #'soy.newcart:product-item products))))))
        (progn
          ;; страница для пустой корзины с автоматическим редиректом на главную
          (soy.newcart:fullpage (list :head (soy.newcart:head-redirect (list :timeout 5
                                                                             :url "/"))
                                      :leftcells (soy.newcart:leftcells-empty)))))))


;; корзина товаров
(defun cart-page ()
  (let ((cart-cookie (hunchentoot:cookie-in "cart"))
        (cart)
        (products)
        (count)
        (pricesum))
    (when cart-cookie
      (setf cart (json:decode-json-from-string cart-cookie))
      (multiple-value-bind (lst cnt sm) (newcart-cart-products cart)
        (setf products (remove-if #'null lst))
        (setf count cnt)
        (setf pricesum sm)))
    (if products
        (default-page
            (soy.newcart:cart-content (list :products (format nil "~{~a~}"
                                                              (mapcar #'soy.newcart:cart-product
                                                                      products))))
            :no-need-cart t)
        ;; страница для пустой корзины с автоматическим редиректом на главную
        (soy.newcart:fullpage (list :head (soy.newcart:head-redirect (list :timeout 5
                                                                           :url "/"))
                                    :header (soy.newcart:header)
                                    :leftcells (soy.newcart:leftcells-empty))))))

;; извлекает данные из ассоциативного списка и нормализовывает
(defun newcart-get-data-from-alist(symbol-name alist)
  (string-trim (list #\Space #\Tab #\Newline) (format nil "~@[~a~]" (cdr (assoc symbol-name alist)))))

;; данные о пользователе
(defun newcart-user (user)
  (let ((phone         (newcart-get-data-from-alist :phone user))  ;; обязательное поле телефон
        ;;два вида доставки курьером и самовывоз (express | pickup)
        (delivery-type (newcart-get-data-from-alist :delivery-type user))
        (name          (newcart-get-data-from-alist :name user)) ;; имя
        (family        (newcart-get-data-from-alist :family user)) ;; фамилия
        (email         (newcart-get-data-from-alist :email user)) ;; email заказчика
        (city          (newcart-get-data-from-alist :city user))
        (addr          (newcart-get-data-from-alist :addr user)) ;; адрес без города
        (courier_comment (newcart-get-data-from-alist :courier--comment user)) ;; комментарий курьеру
        (pickup          (newcart-get-data-from-alist :pickup user)) ;; pickup-1 Левашовский
        (pickup_comment  (newcart-get-data-from-alist :pickup--comment user)) ;; комментарий к заказу
        (payment         (newcart-get-data-from-alist :payment user)) ;; payment_method-1
        (bankaccount     (newcart-get-data-from-alist :bankaccount user)) ;; реквизиты банковского перевода
        (discount-cart   (newcart-get-data-from-alist :discount-card user)) ;; карта ЕКК (true | false)
        (discount-cart-number   (newcart-get-data-from-alist :DISCOUNT-CARD-NUMBER user)) ;; номер карты
        (pickpoint-address (newcart-get-data-from-alist :pickpoint-address user)) ;; адрес постамата
        (ekk nil))
    ;; проставление значений по умолчанию
    (if (string= delivery-type "") (setf delivery-type "pickup"))
    (if (string= payment "") (setf payment "payment_method-1"))
    (if (string= pickpoint-address "") (setf pickpoint-address "Постамат не выбран."))
    ;;Выставляем адрес доставки для филиалов
    (when (string= delivery-type "pickup")
      (setf addr (string-case pickup
                   ("pickup-1" "Левашовский пр., д.12")
                   ("pickup-2" "Петергоф, ул. Ботаническая, д.18, к.3")
                   ("pickup-3" pickpoint-address)
                   (t "Левашовский пр., д.12")))) ;; по умолчанию главный магазин
    (if (or (equal discount-cart t)
            (valid-string-p discount-cart))
        (setf ekk discount-cart-number))
    (values-list (list phone delivery-type name email city addr courier_comment pickup pickup_comment payment bankaccount ekk family))))

;; страница информации об отправленном заказе
(defun thanks-page ()
  (let ((cart) ;; товары
        (user) ;; данные о пользователе
        (products)
        (count)  ;; количество товаров в корзине
        (pricesum) ;; сумма заказа
        (bonuscount)) ;; сумма бонусов
    ;; кукисы пользователя
    (mapcar #'(lambda (cookie)
                (string-case (car cookie)
                  ("cart" (setf cart (json:decode-json-from-string (cdr cookie))))
                  ("user-nc" (setf user (json:decode-json-from-string (cdr cookie))))
                  (t nil)))
            (hunchentoot:cookies-in hunchentoot:*request*))
    ;;если кукисы не пустые
    (when (and (not (null cart))
               (not (null user)))
      (multiple-value-bind (lst cnt sm bc) (newcart-cart-products cart)
        (setf products (remove-if #'null lst))
        (setf count cnt)
        (setf pricesum sm)
        (if (and bc
                 (not (equal bc 0)))
            (setf bonuscount bc))))
    (if (and (not (null products))
             (not (null (newcart-get-data-from-alist :phone user))))
        ;; если в заказе есть валидные товары и телефон пользователя
        ;; генерация идентификатора заказа происходит только если заказ валиден
        (let ((order-id (get-order-id)) ;; генерируем ID заказа
              (deliverysum 0) ;;цена доставки
              (client-mail) ;; текст письма с информацие о заказе для клиента
              (mail-file) ;; информация для ТКС
              (tks-mail) ;; файл с информацией о заказе для ТКС
              (filename)) ;;
          (multiple-value-bind (phone delivery-type name email city addr courier_comment pickup pickup_comment payment bankaccount ekk family)
              (newcart-user user)
            (declare (ignore city))
            ;; Временно доставка 300 на все
            ;; существует два вида доставки: курьером и самовывоз (express | pickup)
            (if  (string= delivery-type "express")
                 (setf deliverysum (yml.get-delivery-price (newcart-cart-products cart))))
            (if  (and (string= delivery-type "pickup")
                      (string= pickup "pickup-3"))
                 (setf deliverysum 100))
            (setf client-mail
                  (soy.sendmail:clientmail
                   (list :datetime (time.get-date-time)
                         :order_id order-id
                         :name (report.convert-name (format nil "~a ~a" name family))
                         :family "" ;; Фамилия не передается отдельно
                         :paytype (string-case payment
                                    ("payment_method-1" "Наличными")
                                    ("payment_method-2" "Кредитной картой")
                                    ("payment_method-3" "Безналичный расчет")
                                    ("payment_method-4" "Банковским переводом")
                                    (t payment))
                         :deliverytype (string-case delivery-type
                                         ("express" "Курьер")
                                         ("pickup" "Самовывоз")
                                         (t delivery-type))
                         :addr addr
                         :bankaccount (if (string= payment "payment_method-4")
                                          bankaccount)
                         :phone phone
                         :ekk ekk
                         :bonuscount (if (and ekk
                                              (not (equal ekk "")))
                                         bonuscount)
                         :bonusname (if bonuscount
                                        (nth (skls.get-count-skls bonuscount)
                                             (list "бонус" "бонуса" "бонусов")))
                         :email email
                         :comment (string-case delivery-type
                                    ("express" courier_comment)
                                    ("pickup" pickup_comment)
                                    (t ""))
                         :articles nil
                         :products products
                         :deliverysum deliverysum
                         :itogo (+ pricesum deliverysum))))
            (setf mail-file (list :order_id order-id
                                  :ekk ekk
                                  :name (report.convert-name (format nil "~a ~a" name family))
                                  :family ""
                                  :addr addr
                                  :isdelivery (string-case delivery-type
                                                ("express" "Доставка")
                                                ("pickup" (if (string= "pickup-3" pickup)
                                                               "PickPoint"
                                                               "Самовывоз"))
                                                (t delivery-type))
                                  :phone phone
                                  :email email
                                  :date (time.get-date)
                                  :time (time.get-time)
                                  :comment (string-case delivery-type
                                             ("express" courier_comment)
                                             ("pickup" pickup_comment)
                                             (t ""))
                                  :products (append products
                                                    (if (string= delivery-type "express")
                                                        (list (list :articul "107209"
                                                                    :cnt "1"
                                                                    :siteprice 300))))))
            (setf filename (format nil "~a_~a.txt" (time.get-short-date) order-id))
            ;;сорханение заказа
            (save-order-text order-id client-mail)
            ;; удаление страных символов
            (setf client-mail (remove-if #'(lambda(c) (< 10000 (char-code c))) client-mail))
            (setf tks-mail (remove-if #'(lambda(c) (< 10000 (char-code c))) (soy.sendmail:mailfile mail-file)))
            (mapcar #'(lambda (email)
                        (send-mail (list email) client-mail filename tks-mail order-id))
                    *conf.emails.cart*)
            ;; сделать валидацию пользовательского email
            (unless (string= email "")
              (send-client-mail (list email) client-mail order-id))
            (soy.newcart:fullpage
             (list :head (soy.newcart:newcart-head
                          (list :thanks
                                (list :orderid order-id
                                      :total pricesum
                                      :delivery deliverysum
                                      :products products)))
                   :header (soy.newcart:header-linked)
                   :leftcells (soy.newcart:thanks
                               (list :sum pricesum
                                     :deliverysum deliverysum
                                     :comment  (let ((comment (format nil "~a"
                                                                      (string-case delivery-type
                                                                        ("express" courier_comment)
                                                                        ("pickup" pickup_comment)
                                                                        (t "")))))
                                                 (when (valid-string-p comment) comment))
                                     :email (if (equal email "") nil email)
                                     :name (if (equal name "") nil (report.convert-name name))
                                     :bonuscount (if (or (equal ekk t)
                                                         (valid-string-p ekk))
                                                      bonuscount)
                                     :bonusname (if bonuscount
                                                    (nth (skls.get-count-skls bonuscount)
                                                         (list "бонус" "бонуса" "бонусов")))
                                     :pickup pickup
                                     :courier (equal delivery-type "express")
                                     :oplata (string-case payment
                                               ("payment_method-1"
                                                "<p class=\"h2\">Оплата наличными</p><p>Вы получите кассовый товарный чек</p>")
                                               ("payment_method-2"
                                                "<p class=\"h2\">Оплата кредитной картой</p>")
                                               ("payment_method-3"
                                                "<p class=\"h2\">Покупка в кредит</p>")
                                               ("payment_method-4"
                                                (format nil "<p class=\"h2\">Оплата по безналичному расчету</p>
                                         <p>Реквизиты:<br/>~a</p>" bankaccount))
                                               (t nil))
                                     :addr (if (string= delivery-type "pickup")
                                               addr
                                               "Левашовский пр., д.12")
                                     :ekk ekk
                                     :map (if (and (equal delivery-type "pickup")
                                                   (equal pickup "pickup-2"))
                                              (soy.newcart:map-botanicheskaya-img)
                                              (soy.newcart:map-levashovskii-img))
                                     :order_id order-id))
                   :rightcells (soy.newcart:rightcells
                                (list :pricesum pricesum
                                      :deliverysum deliverysum
                                      :productscount count
                                      :tovar (newcart-tovar count)
                                      :products (mapcar #'soy.newcart:product-item  products)))))))
        (progn
          (soy.newcart:fullpage (list :head (soy.newcart:head-redirect (list :timeout 5
                                                                             :url "/"))
                                      :header (soy.newcart:header)
                                      :leftcells (soy.newcart:leftcells-empty)))))))


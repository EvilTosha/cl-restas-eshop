;;;; routes.lisp

(in-package #:eshop)

(defun clear ()
  (loop :for var :being :the :symbols :in :eshop.impl.routes :do (unintern var))
  (restas:reconnect-all-routes))

(clear)

;; static content
;; Роуты до статиx файлов
;; Дублирует функционал nginx для развертывания на localhost

(restas:define-route request-static-route-img ("/img/*")
  (let ((full-uri (format nil "~a" (restas:request-full-uri))))
    (pathname (concatenate 'string *path-to-dropbox* "/htimgs/" (subseq full-uri (search "/img/" full-uri))))))

(restas:define-route request-static-route-pic ("/pic/*")
  (let* ((full-uri (format nil "~a" (restas:request-full-uri)))
         (path-to-img (ppcre:regex-replace ".*/pic/(\\w{1,})/(\\d{1,3})(\\d{0,})/(.*)$" full-uri "\\1/\\2/\\2\\3/\\4")))
    (pathname (format nil "~a/~a" (config.get-option "PATHS" "path-to-pics") path-to-img))))

(restas:define-route request-static-route-css ("/css/*")
  (let ((full-uri (format nil "~a" (restas:request-full-uri))))
    (pathname (concatenate 'string *path-to-dropbox* "/htimgs/" (subseq full-uri (search "/css/" full-uri))))))

(restas:define-route request-static-route-js ("/js/*")
  (let ((full-uri (format nil "~a" (restas:request-full-uri))))
    (pathname (concatenate 'string *path-to-dropbox* "/htimgs/" (subseq full-uri (search "/js/" full-uri))))))

(restas:define-route request-route-static-favicon ("/favicon.ico")
  (pathname (concatenate 'string  *path-to-dropbox* "/htimgs/img/favicon.ico")))

(restas:define-route request-route-static-robots ("/robots.txt")
  (merge-pathnames "robots.txt" (config.get-option "CRITICAL" "path-to-conf")))

(restas:define-route request-route-static-yml ("/yml.xml")
  (merge-pathnames "yml.xml" (config.get-option "CRITICAL" "path-to-conf")))

(restas:define-route request-route-static-sitemap ("/sitemap.xml")
  (merge-pathnames "sitemap.xml" (config.get-option "CRITICAL" "path-to-conf")))

(restas:define-route request-route-static-sitemap-index ("/sitemap-index.xml")
  (merge-pathnames "sitemap-index.xml" (config.get-option "CRITICAL" "path-to-conf")))

(restas:define-route request-route-static-sitemap1 ("/sitemap1.xml")
  (merge-pathnames "sitemap1.xml" (config.get-option "CRITICAL" "path-to-conf")))

(restas:define-route request-route-static-sitemap2 ("/sitemap2.xml")
  (merge-pathnames "sitemap2.xml" (config.get-option "CRITICAL" "path-to-conf")))

;; end static content


;; FILTER

(defun test-route-filter ()
  (let* ((request-list (request-list))
         (key (cadr request-list))
         (filter (caddr request-list))
         (grp (gethash key (storage *global-storage*)))
         (fltr (gethash filter (storage *global-storage*))))
    (and (not (null grp))
         (not (null fltr))
         (equal (type-of grp) 'group)
         (equal (type-of fltr) 'filter)
         (equal (key (new-classes.parent fltr)) key))))

(defun route-filter (filter)
  (gethash filter (storage *global-storage*)))

(restas:define-route filter/-route ("/:key/:filter/" :requirement #'test-route-filter)
  (route-filter filter))

(restas:define-route filter-route ("/:key/:filter" :requirement #'test-route-filter)
  (route-filter filter))


;; STORAGE OBJECT

(defun test-route-storage-object ()
  (let ((obj (gethash (cadr (request-list)) (storage *global-storage*))))
    (if (not (null obj))
        (if (and (equal (type-of obj)
                        'group)
                 (not (null (getf (request-get-plist) :vendor))))
            (let ((vendor (getf (request-get-plist) :vendor)))
              (not (= (length (remove-if-not #'(lambda (p)
                                                 (vendor-filter-controller p (request-get-plist)))
                                             (get-recursive-products obj)))
                      0)))
            t)
        nil)))

(defun route-storage-object (key)
  (gethash key (storage *global-storage*)))

(restas:define-route storage-object-route  ("/:key" :requirement #'test-route-storage-object)
  (route-storage-object key))

(restas:define-route storage-object/-route  ("/:key/" :requirement #'test-route-storage-object)
  (route-storage-object key))



;; MAIN
(defun test-get-parameters ()
  t) ;;(null (request-get-plist)))

(restas:define-route main-route ("/" :requirement #'test-get-parameters)
  (main-page-show (request-str)))


;; CATALOG

(restas:define-route catalog-page-route ("/catalog")
  (default-page (catalog.catalog-entity)
      :keywords "Купить компьютер и другую технику вы можете в Цифрах. Цифровая техника в Интернет-магазине 320-8080.ru"
      :description "каталог, компьютеры, купить компьютер, компьютерная техника, Петербург, Спб, Питер, Санкт-Петербург, продажа компьютеров, магазин компьютерной техники, магазин компьютеров, интернет магазин компьютеров, интернет магазин компьютерной техники, продажа компьютерной техники, магазин цифровой техники, цифровая техника, Цифры, 320-8080"
      :title "Каталог интернет-магазина: купить компьютер, цифровую технику, комплектующие в Санкт-Петербурге"))

(restas:define-route sitemap-page-route ("/sitemap")
  (default-page (catalog.sitemap-page)
      :keywords "Купить компьютер и другую технику вы можете в Цифрах. Цифровая техника в Интернет-магазине 320-8080.ru"
      :description "каталог, компьютеры, купить компьютер, компьютерная техника, Петербург, Спб, Питер, Санкт-Петербург, продажа компьютеров, магазин компьютерной техники, магазин компьютеров, интернет магазин компьютеров, интернет магазин компьютерной техники, продажа компьютерной техники, магазин цифровой техники, цифровая техника, Цифры, 320-8080"
      :title "Каталог интернет-магазина: купить компьютер, цифровую технику, комплектующие в Санкт-Петербурге"))


;; CART & CHECKOUTS & THANKS

(restas:define-route cart-route ("/cart")
  (cart-page))

(restas:define-route checkout-route ("/checkout")
  (newcart-show))

(restas:define-route checkout-post-route ("/checkout" :method :post)
  (newcart-show))

(restas:define-route checkout0-route ("/checkout0")
  (newcart-show))

(restas:define-route checkout1-route ("/checkout1")
  (newcart-show))

(restas:define-route checkout2-route ("/checkout2")
  (newcart-show))

(restas:define-route checkout3-route ("/checkout3")
  (newcart-show))

(restas:define-route thanks-route ("/thanks")
  (thanks-page))


;; GATEWAY

;; (restas:define-route gateway-route ("/gateway")
;;   (gateway-page))

(restas:define-route gateway/post-route ("/gateway" :method :post)
  (gateway-page))


;; (restas:define-route gateway/-route ("/gateway/")
;;   (gateway-page))

;; (restas:define-route gateway/post/-route ("/gateway/" :method :post)
;;   (gateway-page))


;; SEARCH

(restas:define-route search-route ("/search")
  (search-page))


;; YML

(restas:define-route yml-route ("/yml")
  (yml-page))

(restas:define-route yml/-route ("/yml/")
  (yml-page))

(restas:define-route parseryml-route ("/parseryml")
  (yml-page-for-parser))

;; ARTICLES
;;TODO возможно проверять входные тэги
(defun test-article-get-parameters ()
  t)

;;проверяем есть ли такая статья
(defun test-route-article-object ()
  (not (null (gethash (caddr (request-list)) *storage-articles*))))

;;архив матерьялов
(restas:define-route article-route ("/articles" :requirement #'test-article-get-parameters)
  (articles-page (request-get-plist)))

;;список статей
(restas:define-route article-papers-route ("/articles/papers" :requirement #'test-article-get-parameters)
  (let ((request-get-plist (request-get-plist)))
    (if (null (getf request-get-plist :tags))
        (setf (getf request-get-plist :tags) "Статьи"))
    (articles-page request-get-plist)))

;;список новостей
(restas:define-route article-news-route ("/articles/news" :requirement #'test-article-get-parameters)
  (let ((request-get-plist (request-get-plist)))
    (if (null (getf request-get-plist :tags))
        (setf (getf request-get-plist :tags) "Новости"))
    (articles-page request-get-plist)))

;;список обзоры
(restas:define-route article-review-route ("/articles/reviews" :requirement #'test-article-get-parameters)
  (let ((request-get-plist (request-get-plist)))
    (if (null (getf request-get-plist :tags))
        (setf (getf request-get-plist :tags) "Обзоры"))
    (articles-page request-get-plist)))

;;конкретная статья
(restas:define-route article/-key-route ("/articles/:key" :requirement #'test-route-article-object)
  (gethash (caddr (request-list)) *storage-articles*))


;; 404

;;необходимо отдавать 404 ошибку для несуществеющих страниц
(restas:define-route not-found-route ("*any")
  (log5:log-for test "error 404: ~a" any)
  (restas:abort-route-handler
   (babel:string-to-octets
    (default-page (catalog.sitemap-page t)
        :keywords "Купить компьютер и другую технику вы можете в Цифрах. Цифровая техника в Интернет-магазине 320-8080.ru"
        :description "каталог, компьютеры, купить компьютер, компьютерная техника, Петербург, Спб, Питер, Санкт-Петербург, продажа компьютеров, магазин компьютерной техники, магазин компьютеров, интернет магазин компьютеров, интернет магазин компьютерной техники, продажа компьютерной техники, магазин цифровой техники, цифровая техника, Цифры, 320-8080"
        :title "Каталог интернет-магазина: купить компьютер, цифровую технику, комплектующие в Санкт-Петербурге")
    :encoding :utf-8)
   :return-code hunchentoot:+http-not-found+
   :content-type "text/html"))

(restas:define-route request-route ("/request")
  (oneclickcart.make-common-order (request-get-plist)))

(restas:define-route request1-route ("/request1")
  (oneclickcart.page (request-get-plist)))



(defun bts-view-page ()
  (list :content
        "<script language=\"JavaScript\" type=\"text/javascript\">
var i,y,x=\"3c7461626c6520636c6173733d226261636b5f746f5f7363686f6f6c223e0d0a3c74723e0d0a3c746420636c6173733d226267223e3c2f74643e0d0a3c74643e0d0a3c696d672069643d22496d6167652d4d6170735f3232303131303832393039323034313822207372633d222f696d672f617567757374323031312f6261636b5f746f5f7363686f6f6c2e6a706722207573656d61703d2223496d6167652d4d6170735f32323031313038323930393230343138222f3e0d0a3c6d6170206e616d653d22496d6167652d4d6170735f32323031313038323930393230343138223e0d0a0d0a3c617265612073686170653d22726563742220636f6f7264733d22313130362c3130372c313232322c3134312220687265663d222f3136363538342220616c743d224170706c65204950414432203136474220336722207469746c653d224170706c65204950414432203136474220336722202020202f3e0d0a3c617265612073686170653d22726563742220636f6f7264733d223933342c36372c313232322c3130332220687265663d222f3136363538342220616c743d224170706c65204950414432203136474220336722207469746c653d224170706c65204950414432203136474220336722202020202f3e0d0a3c617265612073686170653d22706f6c792220636f6f7264733d223333382c3134352c3932362c3134342c3935322c3136332c3931302c3137382c3832392c3134352c3735342c3231302c3730392c3230392c3730362c3233312c3934342c3331362c3935372c3336352c3839352c3433362c3839332c3438312c3935342c3434302c3934392c3933322c3932332c3934382c3335372c3935312c3332362c3932342c3332392c3136362c2220687265663d222f3136363538342220616c743d224170706c65204950414432203136474220336722207469746c653d224170706c652049504144322031364742203367222020202f3e0d0a0d0a3c617265612073686170653d22706f6c792220636f6f7264733d223833312c313031392c3936392c313031332c3938352c313033332c3938362c313235312c3937342c313237322c3832352c313237352c2220687265663d222f3135333130322220616c743d225765737465726e204469676974616c20315462205744424759533030313042424b22207469746c653d225765737465726e204469676974616c20315462205744424759533030313042424b222020202f3e0d0a3c617265612073686170653d22726563742220636f6f7264733d22313031302c313032372c313234362c313038382220687265663d222f3135333130322220616c743d225765737465726e204469676974616c20315462205744424759533030313042424b22207469746c653d225765737465726e204469676974616c20315462205744424759533030313042424b22202020202f3e0d0a3c617265612073686170653d22726563742220636f6f7264733d22313133322c313039322c313234322c313132392220687265663d222f3135333130322220616c743d225765737465726e204469676974616c20315462205744424759533030313042424b22207469746c653d225765737465726e204469676974616c20315462205744424759533030313042424b22202020202f3e0d0a0d0a3c617265612073686170653d22726563742220636f6f7264733d223233372c313030302c3335322c313033382220687265663d222f3136343138372220616c743d2243616e6f6e20506f77657273686f7420413232303022207469746c653d2243616e6f6e20506f77657273686f7420413232303022202020202f3e0d0a3c617265612073686170653d22726563742220636f6f7264733d2238382c3936362c3334342c313030312220687265663d222f3136343138372220616c743d2243616e6f6e20506f77657273686f7420413232303022207469746c653d2243616e6f6e20506f77657273686f7420413232303022202020202f3e0d0a3c617265612073686170653d22706f6c792220636f6f7264733d223132322c3630372c3133382c3630312c3239362c3634382c3233302c3933392c3230382c3934362c34302c3839372c2220687265663d222f3136343138372220616c743d2243616e6f6e20506f77657273686f7420413232303022207469746c653d2243616e6f6e20506f77657273686f74204132323030222020202f3e0d0a0d0a3c617265612073686170653d22726563742220636f6f7264733d223437322c313138332c3639372c313231382220687265663d222f3136323930332220616c743d224854432057696c6466697265205322207469746c653d224854432057696c6466697265205322202020202f3e0d0a3c617265612073686170653d22726563742220636f6f7264733d223538372c313231382c3730302c313235332220687265663d222f3136323930332220616c743d224854432057696c6466697265205322207469746c653d224854432057696c6466697265205322202020202f3e0d0a3c617265612073686170653d22706f6c792220636f6f7264733d223337392c313135362c3430392c313136322c3535382c313431392c3535312c313434312c3432332c313532322c3338372c313531352c3234332c313236332c3234382c313233322c2220687265663d222f3136323930332220616c743d224854432057696c6466697265205322207469746c653d224854432057696c64666972652053222020202f3e0d0a0d0a3c617265612073686170653d22726563742220636f6f7264733d2235312c313835382c3334302c323130332220687265663d222f3134383835392220616c743d224c656e6f766f20496465615061642047353635413122207469746c653d224c656e6f766f20496465615061642047353635413122202020202f3e0d0a3c617265612073686170653d22706f6c792220636f6f7264733d223635302c313631342c3734362c313739342c3736392c313830312c313232382c323036322c3935322c323233342c3832342c323231352c3737352c323236352c3734322c323330312c3731352c323332342c3731362c323335332c3733382c323336362c3539342c323431332c3539332c323433342c3630392c323434332c3734362c323431342c3738302c323433332c3830332c323435302c3837312c323433372c3937332c323338392c313035352c323333372c313133342c323238332c313233382c323234382c3833342c323935312c3238342c323633352c3238322c323631312c3236332c323632352c35362c323634302c2220687265663d222f3134383835392220616c743d224c656e6f766f20496465615061642047353635413122207469746c653d224c656e6f766f204964656150616420473536354131222020202f3e0d0a3c617265612073686170653d22726563742220636f6f7264733d223232342c323130372c3334302c323134332220687265663d222f3134383835392220616c743d224c656e6f766f20496465615061642047353635413122207469746c653d224c656e6f766f20496465615061642047353635413122202020202f3e0d0a0d0a3c617265612073686170653d22726563742220636f6f7264733d223835352c333035332c3937312c333038392220687265663d222f3136333437302220616c743d224e6f6b69612043322d303122207469746c653d224e6f6b69612043322d303122202020202f3e0d0a3c617265612073686170653d22706f6c792220636f6f7264733d223734342c333031392c3935372c333032302c3935392c333035302c3734392c333035302c2220687265663d222f3136333437302220616c743d224e6f6b69612043322d303122207469746c653d224e6f6b69612043322d3031222020202f3e0d0a3c617265612073686170653d22706f6c792220636f6f7264733d223532382c323835312c3733312c333038352c3732342c333131382c3635352c333137352c3632332c333137302c3433312c323934342c3432392c323931382c2220687265663d222f3136333437302220616c743d224e6f6b69612043322d303122207469746c653d224e6f6b69612043322d3031222020202f3e0d0a0d0a3c617265612073686170653d22706f6c792220636f6f7264733d223236362c333032392c3535352c333135392c3333312c333637352c34312c333535302c2220687265663d222f3136303331322220616c743d2250726573746967696f204d756c746950616420504d50353037304322207469746c653d2250726573746967696f204d756c746950616420504d503530373043222020202f3e0d0a3c617265612073686170653d22706f6c792220636f6f7264733d2235392c333731332c38352c333730322c3331392c333639392c3333392c333732302c3333332c333831302c3331322c333832392c37362c333832342c35352c333831312c2220687265663d222f3136303331322220616c743d2250726573746967696f204d756c746950616420504d50353037304322207469746c653d2250726573746967696f204d756c746950616420504d503530373043222020202f3e0d0a3c617265612073686170653d22706f6c792220636f6f7264733d223232392c333832392c3333352c333833312c3333362c333835392c3232392c333835382c2220687265663d222f3136303331322220616c743d2250726573746967696f204d756c746950616420504d50353037304322207469746c653d2250726573746967696f204d756c746950616420504d503530373043222020202f3e0d0a0d0a3c617265612073686170653d22706f6c792220636f6f7264733d22313237342c333033312c3638372c333331302c3635362c333339312c3632342c333633322c3635312c333639322c3634302c333733382c3932302c343232382c3936322c343232392c313037302c343339362c313134392c343431302c313237392c343333352c2220687265663d222f3134343930312220616c743d2253616d73756e67205343582d3332303022207469746c653d2253616d73756e67205343582d33323030222020202f3e0d0a3c617265612073686170653d22706f6c792220636f6f7264733d223637302c343239322c3936322c343239342c3936362c343331382c3637312c343332312c2220687265663d222f3134343930312220616c743d2253616d73756e67205343582d3332303022207469746c653d2253616d73756e67205343582d33323030222020202f3e0d0a3c617265612073686170653d22706f6c792220636f6f7264733d223835392c343333312c3936382c343333322c3936392c343336302c3835382c343336312c2220687265663d222f3134343930312220616c743d2253616d73756e67205343582d3332303022207469746c653d2253616d73756e67205343582d33323030222020202f3e0d0a0d0a3c617265612073686170653d22726563742220636f6f7264733d223232312c3432342c3232392c3433322220687265663d222f3939393131312220616c743d222623313035353b2623313038363b2623313038373b2623313038383b2623313038363b2623313037333b2623313039313b2623313038313b2d2623313038323b2623313037323b212220207469746c653d222623313035353b2623313038363b2623313038373b2623313038383b2623313038363b2623313037333b2623313039313b2623313038313b2d2623313038323b2623313037323b2122202f3e0d0a3c617265612073686170653d22726563742220636f6f7264733d22313230352c3438372c313231332c3439352220687265663d222f3833343738362220616c743d222623313035333b2623313037323b2623313037383b2623313038343b2623313038303b202623313038393b2623313130323b2623313037363b2623313037323b212220207469746c653d222623313035333b2623313037323b2623313037383b2623313038343b2623313038303b202623313038393b2623313130323b2623313037363b2623313037323b2122202f3e0d0a3c617265612073686170653d22726563742220636f6f7264733d223134302c313136312c3134382c313136392220687265663d222f3838383737372220616c743d222623313035353b2623313038363b2623313038373b2623313038383b2623313038363b2623313037333b2623313039313b2623313038313b202623313038393b2623313130323b2623313037363b2623313037323b2122207469746c653d222623313035353b2623313038363b2623313038373b2623313038383b2623313038363b2623313037333b2623313039313b2623313038313b202623313038393b2623313130323b2623313037363b2623313037323b2122202f3e0d0a3c617265612073686170653d22726563742220636f6f7264733d223839382c313337372c3930362c313338352220687265663d222f3731313236352220616c743d222623313035393b2623313037343b2623313037373b2623313038383b2623313037373b2623313038353b3f202623313035303b2623313038333b2623313038303b2623313038323b2623313037323b2623313038313b2122207469746c653d222623313035393b2623313037343b2623313037373b2623313038383b2623313037373b2623313038353b3f202623313035303b2623313038333b2623313038303b2623313038323b2623313037323b2623313038313b2122202f3e0d0a3c617265612073686170653d22726563742220636f6f7264733d223330352c313730302c3331332c313730382220687265663d222f3933383131312220616c743d222623313034363b2623313038343b2623313038303b202623313038393b2623313038323b2623313038363b2623313038383b2623313037373b2623313037373b2122207469746c653d222623313034363b2623313038343b2623313038303b202623313038393b2623313038323b2623313038363b2623313038383b2623313037373b2623313037373b2122202f3e0d0a3c617265612073686170653d22726563742220636f6f7264733d22313134342c323831312c313135322c323831392220687265663d222f3737373838382220616c743d222623313035373b2623313038343b2623313037373b2623313038333b2623313037373b2623313037373b202623313038323b2623313038333b2623313038303b2623313038323b2623313037323b2623313038313b212220207469746c653d222623313035373b2623313038343b2623313037373b2623313038333b2623313037373b2623313037373b202623313038323b2623313038333b2623313038303b2623313038323b2623313037323b2623313038313b2122202f3e0d0a3c617265612073686170653d22726563742220636f6f7264733d223134362c343231312c3135342c343231392220687265663d222f3939393737372220616c743d222623313035383b2623313037373b2623313037333b2623313037373b202623313038373b2623313038363b2623313037343b2623313037373b2623313037393b2623313037373b2623313039303b2122207469746c653d222623313035383b2623313037373b2623313037333b2623313037373b202623313038373b2623313038363b2623313037343b2623313037373b2623313037393b2623313037373b2623313039303b2122202f3e0d0a3c617265612073686170653d22726563742220636f6f7264733d22313234312c313736352c313234392c313737332220687265663d222f3135383533332220616c743d222623313035353b2623313038363b2623313039353b2623313039303b2623313038303b202623313038373b2623313038363b2623313038373b2623313037323b2623313038333b2122207469746c653d222623313035353b2623313038363b2623313039353b2623313039303b2623313038303b202623313038373b2623313038363b2623313038373b2623313037323b2623313038333b2122202f3e0d0a3c617265612073686170653d22726563742220636f6f7264733d2233362c333036302c34342c333036382220687265663d222f3134353330342220616c743d222623313034323b2623313038363b2623313037393b2623313038343b2623313038363b2623313037383b2623313038353b2623313038363b2c202623313039353b2623313039303b2623313038363b202623313038373b2623313038383b2623313038303b2623313037393b202623313037393b2623313037363b2623313037373b2623313038393b2623313130303b2122207469746c653d222623313034323b2623313038363b2623313037393b2623313038343b2623313038363b2623313037383b2623313038353b2623313038363b2c202623313039353b2623313039303b2623313038363b202623313038373b2623313038383b2623313038303b2623313037393b202623313037393b2623313037363b2623313037373b2623313038393b2623313130303b2122202f3e0d0a3c617265612073686170653d22726563742220636f6f7264733d223530332c333835312c3531312c333835392220687265663d222f3134303438352220616c743d222623313035333b2623313037323b2623313037363b2623313037373b2623313037383b2623313037363b2623313037323b202623313039313b2623313038343b2623313038303b2623313038383b2623313037323b2623313037373b2623313039303b202623313038373b2623313038363b2623313038393b2623313038333b2623313037373b2623313037363b2623313038353b2623313037373b2623313038313b212220207469746c653d222623313035333b2623313037323b2623313037363b2623313037373b2623313037383b2623313037363b2623313037323b202623313039313b2623313038343b2623313038303b2623313038383b2623313037323b2623313037373b2623313039303b202623313038373b2623313038363b2623313038393b2623313038333b2623313037373b2623313037363b2623313038353b2623313037373b2623313038313b2122202f3e0d0a3c2f6d61703e0d0a0d0a3c2f74643e0d0a3c2f74723e3c2f7461626c653e\";y='';for(i=0;i<x.length;i+=2){y+=unescape('%'+x.substr(i,2));}document.write(y);
</script>"))

(restas:define-route back-to-school-route ("/back_to_school")
  (root:main (bts-view-page)))

(restas:define-route back-to-school/-route ("/back_to_school/")
  (root:main (bts-view-page)))

(restas:define-route elka-2011-route ("/elka2012")
  (soy.elka2012:base (list :days (time.get-delta-new-year-date)
                           :orders (if *order-id* (ceiling (- *order-id* 66070) 4) 0)
                           :sharebuttons (soy.articles:share-buttons))))


(restas:define-route elka-2011/-route ("/elka2012/")
  (soy.elka2012:base (list :days (time.get-delta-new-year-date)
                           :orders (if *order-id* (ceiling (- *order-id* 66070) 4) 0)
                           :sharebuttons (soy.articles:share-buttons))))

(restas:define-route holidays/-route ("/holidays-2012/")
  (soy.holidays:body))

(restas:define-route holidays-route ("/holidays-2012")
  (soy.holidays:body))


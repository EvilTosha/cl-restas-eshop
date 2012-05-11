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
 ;; (checkout-page-0))

(restas:define-route checkout1-route ("/checkout1")
  (newcart-show))
  ;; (checkout-page-1))

(restas:define-route checkout2-route ("/checkout2")
  (newcart-show))
  ;; (checkout-page-2))

(restas:define-route checkout3-route ("/checkout3")
  (newcart-show))
  ;; (checkout-page-3))

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

;; (restas:define-route not-found-route-404 ("/404.html")
;;   (restas:abort-route-handler
;;    (babel:string-to-octets
;;     (default-page
;;         (static:main (list :menu (menu "") :subcontent (error-404:content))))
;;     :encoding :utf-8)
;;    :return-code hunchentoot:+http-not-found+
;;    :content-type "text/html"))

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

(restas:define-route compare-route ("/compare")
	 (soy.compare:compare-page
		(list :keywords "" ;;keywords
					:description "" ;;description
					:title ""
					:header (root:header (append (list :logged (root:notlogged)
																						 :cart (root:cart))
																			 (main-page-show-banner "line" (banner *main-page.storage*))))
					:footer (root:footer))))

(in-package #:eshop)
(print "ESHOP config")

;; PATH
(defparameter *path-to-dropbox* (format nil "~aDropbox" (user-homedir-pathname)))
(export '*path-to-dropbox*)
(defparameter *path-to-logs* (format nil "~aeshop-logs" (user-homedir-pathname)))
(export '*path-to-logs*)

;; ORDER
(defparameter *path-order-id-file* "order-id.txt")
(export '*path-order-id-file*)

;; Список email для рассылки писем от ошибках выгрузки 1с
(defvar *conf.emails.gateway.warn* (list "Supplers@alpha-pc.com"
                                         "web_design@alpha-pc.com"
                                         "wolforus@gmail.com"
                                         "slamly@gmail.com"))

(defvar *conf.emails.xls.warn* (list "wolforus@gmail.com"
                                     "web_design@alpha-pc.com"))

;; Список email для заказов
(defvar *conf.emails.cart* (list "internetorder@alpha-pc.com"
                                 "shop@320-8080.ru"
                                 "zakaz320@yandex.ru"
                                 "slamly@gmail.com"
                                 "wolforus@gmail.com"))


(config.parse-config)

(defun compile-templates ()
  (mapcar #'(lambda (fname)
              (let ((pathname (merge-pathnames (pathname fname) (config.get-option "PATHS" "path-to-templates"))))
                (format t "~&compile-template: ~a" pathname)
                (closure-template:compile-template :common-lisp-backend pathname)))
          '("index.html"            "product.soy"            "product-accessories.html"
            "product-reviews.html"  "product-simulars.html"   "product-others.html"
            "catalog.html"          "catalog-in.html"         "catalog-staff.html"
            "footer.html" "holidays.soy"
            "menu.html"             "banner.html"
            "notlogged.html"
            "cart-widget.html"      "cart.html"               "checkout.html"
            "admin.html"            "admin-table.soy"
            "search.html"
            "agent.html"            "update.html"
            "header.html"           "static.html"
            "yml.html"                "fullfilter.html"
            "sendmail.html"
            "404.html"
            "articles.soy"         "sitemap.soy"            "newcart.soy"
            "oneclickcart.soy"
             "buttons.soy"
            "main-page.soy"
            "new-catalog.soy"
            "class_forms.soy"
            "admin.soy"
            "elka2012.soy"
            )))

(print "Compiling all templates")
(compile-templates)
(print "Compiling all templates finish")

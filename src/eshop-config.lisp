;;;; eshop-config.lisp

(in-package #:eshop)

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
    ;; (let ((templates-path (config.get-option "PATHS" "path-to-templates"))
  ;;       (templates))
  ;;   (mapcar #'(lambda (x)
  ;;               (if (not (cl-fad:directory-pathname-p x))
  ;;                   (push x templates)))
  ;;           (directory (format nil "~a/*" templates-path)))
  ;;   (mapcar #'(lambda (file)
  ;;               (log5:log-for info-console "Load article: ~a" file)
  ;;               (unserialize (format nil "~a" file) (make-instance 'article :ctype ctype)))
  ;;           files)))

  (mapcar #'(lambda (fname)
              (let ((pathname (merge-pathnames fname (config.get-option "PATHS" "path-to-templates"))))
                (format t "~&compile-template: ~a" pathname)
                (closure-template:compile-template :common-lisp-backend pathname)))
          '("admin.soy"
            "admin-table.soy"
            "articles.soy"
            "buttons.soy"
            "catalog.soy"
            "class_forms.soy"
            "compare.soy"
            "footer.soy"
            "fullfilter.soy"
            "header.soy"
            "index.soy"
            "main-page.soy"
            "menu.soy"
            "newcart.soy"
            "new-catalog.soy"
            "oneclickcart.soy"
            "product.soy"
            "sendmail.soy"
            "sitemap.soy"
            "static.soy"
            "yml.soy")))

(print "Compiling all templates")
(compile-templates)
(print "Compiling all templates finish")

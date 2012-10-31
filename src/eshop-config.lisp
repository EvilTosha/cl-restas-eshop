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

(config.parse-config)

(defun compile-templates ()
  (mapcar #'(lambda (fname)
              (let ((pathname (merge-pathnames fname (config.get-option "PATHS" "path-to-templates"))))
                (format t "~&compile-template: ~a" pathname)
                (closure-template:compile-template :common-lisp-backend pathname)))
          '("admin.soy"
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
            "product.soy"
            "sendmail.soy"
            "sitemap.soy"
            "static.soy"
            "yml.soy"
            "404.soy")))

(print "Compiling all templates")
(compile-templates)
(print "Compiling all templates finish")

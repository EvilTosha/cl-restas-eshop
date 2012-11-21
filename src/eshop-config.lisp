;;;; eshop-config.lisp

(in-package #:eshop)

(config.parse-config)

(defun compile-templates ()
  (mapcar #'(lambda (fname)
              (let ((pathname (merge-pathnames fname (config.get-option :paths :path-to-templates))))
                (format t "~&compile-template: ~A" pathname)
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

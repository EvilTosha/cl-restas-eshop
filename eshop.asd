(defsystem eshop
  :depends-on (#:restas #:cl-json #:arnesi #:closure-template #:log5 #:string-case #:alexandria #:cl-csv :cl-mime :data-sift :cl-heap)
  :components
  ((:module "src"
            :serial t
            :components
            ((:module "patches"
                      :components
                      ((:module "closure-templates"
                                :components
                                ((:file "common-lisp-backend")))
                       (:module "cl-mime"
                                :components ((:file "encoding")
                                             (:file "headers")))))
             (:file "packages")
             (:file "search-tips")
             (:file "images") ;; imagemagic
             (:file "config")
             (:file "time")
             (:file "eshop-config")
             (:file "errors")
             (:file "log")
             (:file "servo")
             (:file "routes")
             (:file "render")
             (:file "cart")
             (:file "search")
             (:file "xls")  ;;необходима xls2csv | sudo apt-get install catdoc
             (:file "yml")
             (:file "articles")
             (:file "sklonenie")
             (:file "newcart")
             (:file "sitemap")
             (:file "rename")
             (:file "catalog")
             (:file "prerender")
             (:file "storage")
             (:file "slots")
             (:file "backup")
             (:file "class-core")
             (:file "classes")
             (:file "main-page")
             (:file "filters")
             (:file "marketing-filters")
             (:file "oneclickcart")
             (:file "static-pages")
             (:file "admin")
             (:file "gateway")
             (:file "sendmail")
             (:file "email")
             (:file "groupd")
             (:file "cartrige")
             (:file "report")
             (:file "black-list")
             (:module "cl-cron"
                      :components ((:file "packages")
                                   (:file "cl-cron"))
                      :depends-on ("classes"))
             (:file "cron")
             (:file "debug")))))

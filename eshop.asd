(defsystem eshop
  :depends-on (#:restas #:cl-json #:arnesi #:closure-template #:log5 #:py-configparser #:string-case #:alexandria #:cl-csv :cl-mime)
  :components
  ((:module "src"
            :components
            ((:module "patches"
                      :components
                      ((:module "closure-templates"
                                :components
                                ((:file "common-lisp-backend")
                                 (:file "parse")))
                       (:module "cl-mime"
                                :components ((:file "encoding")))))
             (:file "packages" :depends-on ("patches"))
             (:file "images" :depends-on ("packages")) ;; imagemagic
             (:file "config" :depends-on ("images"))
             (:file "time" :depends-on ("config"))
             (:file "eshop-config" :depends-on ("time"))
             (:file "errors" :depends-on ("eshop-config"))
             (:file "log" :depends-on ("errors"))
             (:file "servo" :depends-on ("log"))
             (:file "routes" :depends-on ("servo"))
             (:file "render" :depends-on ("routes"))
             (:file "cart" :depends-on ("render"))
             (:file "search" :depends-on ("cart"))
             (:file "xls" :depends-on ("search"))  ;;необходима xls2csv | sudo apt-get install catdoc
             (:file "yml" :depends-on ("xls"))
             (:file "articles" :depends-on ("yml"))
             (:file "sklonenie" :depends-on ("articles"))
             (:file "newcart" :depends-on ("sklonenie"))
             (:file "sitemap" :depends-on ("newcart"))
             (:file "rename" :depends-on ("sitemap"))
             (:file "catalog" :depends-on ("rename" "images"))
             (:file "prerender" :depends-on ("catalog"))
             (:file "storage" :depends-on ("prerender"))
             (:file "slots" :depends-on ("storage"))
             (:file "backup" :depends-on ("slots"))
             (:file "class-core" :depends-on ("backup"))
             (:file "classes" :depends-on ("class-core"))
             (:file "main-page" :depends-on ("classes"))
             (:file "filters" :depends-on ("main-page"))
             (:file "marketing-filters" :depends-on ("filters"))
             (:file "oneclickcart" :depends-on ("marketing-filters"))
             (:file "static-pages" :depends-on ("images" "log"))
             (:file "admin" :depends-on ("filters" "cron"))
             (:file "gateway" :depends-on ("admin"))
             (:file "sendmail" :depends-on ("gateway"))
             (:file "email" :depends-on ("sendmail"))
             (:file "groupd" :depends-on ("email"))
             (:file "cartrige" :depends-on ("groupd"))
             (:file "report" :depends-on ("cartrige"))
             (:module "cl-cron"
                      :components ((:file "packages")
                                   (:file "cl-cron"))
                      :depends-on ("classes"))
             (:file "cron" :depends-on ("cl-cron"))
             (:file "debug" :depends-on ("packages"))))))

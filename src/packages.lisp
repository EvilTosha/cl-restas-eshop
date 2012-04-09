;; TODO fix exports

;; (print "restas:define-module CL-ESHOP")
(restas:define-module #:eshop
    (:use :cl
          :closure-template
          :anaphora
          :split-sequence
          :cl-ppcre
          :json
          :cl-fad)
  (:import-from :arnesi :parse-float)
  (:import-from :alexandria :read-file-into-string)
  (:export :*eshop-config*
					 :config.parse-config
					 :compile-templates
					 :name
					 :unserialize
					 :plist-representation
					 :*path-to-bkps*
					 :*path-to-pics*
					 :*path-to-conf*
					 :*path-to-tpls*))


(defpackage #:wolfor-stuff
  (:use #:cl
        #:eshop))

(defpackage #:eshop-test
  (:use #:cl
        #:eshop
        #:wolfor-stuff))


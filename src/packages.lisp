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
  (:export :config.parse-config
					 :config.get-option
					 :config.set-option
					 :config.has-option-p
					 :compile-templates
					 :name
					 :unserialize
					 :*path-to-bkps*
					 :*path-to-pics*
					 :*path-to-conf*))


(defpackage #:eshop-test
  (:use #:cl
        #:eshop))


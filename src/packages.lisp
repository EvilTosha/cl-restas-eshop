;; TODO fix exports

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
					 :servo.compile-soy
					 :dtd
					 ;;old
					 :name
					 :unserialize))


(defpackage #:eshop-test
  (:use #:cl
        #:eshop))


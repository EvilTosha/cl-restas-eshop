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
					 :sklonenie.restore
					 :new-classes.unserialize-all
					 :gateway.restore-history
					 :static-pages.restore
					 :articles.restore
					 :main-page.restore
           :cartrige.restore))

;;; registering classes for proper compilation of methods
;; articles.lisp
(defclass article () ())
;; main-page.lisp
(defclass main-page-storage () ())
(defclass main-page-product () ())
;; xls.lisp
(defclass nko () ())
;; classes.lisp / new-classes.lisp
;; TODO: get rid of classes.lisp
(defclass group () ())
(defclass product () ())
(defclass filter () ())
(defclass group-filter () ())
;; oneclickcart.lisp
(defclass oneclickcart.answer () ())
;; list-filters.lisp
(defclass field-filter () ())
;; storage.lisp
(defclass global-storage () ())
;; cartrige.lisp
(defclass printer () ())

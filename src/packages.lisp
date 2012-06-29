;;;; packages.lisp

(defpackage :pics
  (:use :cl :anaphora)
  (:export :get-dimensions
           :style-for-resize))

;;; TODO: why not defpackage? Edit if needed
(restas:define-module #:eshop
    (:use
     ;; system and libs' packages
     :cl
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
           :class-core.unserialize-all
           :gateway.restore-history
           :static-pages.restore
           :articles.restore
           :main-page.restore
           :cartrige.restore))

(in-package #:eshop)
;;; registering classes for proper compilation of methods
;; articles.lisp
(defclass article () ())
;; main-page.lisp
(defclass main-page-storage () ())
(defclass main-page-product () ())
;; xls.lisp
(defclass nko () ())
;; classes.lisp / class-core.lisp
;; TODO: get rid of classes.lisp
(defclass group () ())
(defclass product () ())
(defclass filter () ())
(defclass vendor () ())
(defclass group-filter () ())
;; oneclickcart.lisp
(defclass oneclickcart.answer () ())
;; filters.lisp
(defclass field-filter () ())
;; cartrige.lisp
(defclass printer () ())

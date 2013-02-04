;;;; packages.lisp

(defpackage :search-tips
  (:use :cl)
  (:export :search-tip
           :search-tips
           :build-search-tips
           :max-k-tips-by-prefix
           :tip
           :info
           :tips
           :weight))

;;; TODO: make separated package eshop-core (class-core, slots, backup, etc.)
(restas:define-module #:eshop
    (:use
     ;; system and libs' packages
     :cl
     :closure-template
     :anaphora
     :split-sequence
     :cl-ppcre
     :json
     :cl-fad
     :string-case
     ;; :sendmail
     )
  (:import-from :arnesi :parse-float)
  (:import-from :alexandria :read-file-into-string)
  (:import-from :alexandria :rcurry)
  (:export :config.parse-config
           :config.get-option
           :servo.compile-soy
           :xls.update-options-from-xls
           :sklonenie.restore
           :class-core.unserialize-all
           :gateway.load
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

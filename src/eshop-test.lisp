(asdf:load-system :drakma)

(defpackage :eshop-test
  (:use :cl :drakma))

(in-package :eshop-test)

(setf *header-stream* nil) ;;*standard-output*)
(setf *drakma-default-external-format* :utf-8)

;; (defvar *eshop-test.server* "www.320-8080.ru")
(defvar *eshop-test.server* "localhost:8080")

(defun test-main-page ()
  (let ((*header-stream* *standard-output*)
        (url (format nil "http://~a/" *eshop-test.server*))
        (result)
        (header))
    (setf header (with-output-to-string (*standard-output*)
                   (setf result (http-request url
                                              :method :get
                                              ;; :parameters (create-register-post user secret-value)
                                              ;; :cookie-jar *cookie*
                                              ))))
    header))


(defun test-gateway ()
  )

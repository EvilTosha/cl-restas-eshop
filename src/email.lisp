;;;; email.lisp

(in-package #:eshop)

;; TODO: move lists of emails to config

;; Список email для рассылки писем от ошибках выгрузки 1с
(defvar *conf.emails.gateway.warn* (list "Supplers@alpha-pc.com"
                                         "web_design@alpha-pc.com"
                                         "wolforus@gmail.com"
                                         "slamly@gmail.com"))

(alexandria:define-constant +xls-warn-emails+ (list "wolforus@gmail.com"
                                                    "web_design@alpha-pc.com")
  :test (constantly t)
  :documentation "List of emails to which warnings about double products will be sent")

(alexandria:define-constant +email-warn-template+
    (make-instance 'sendmail:email
                   :from "shop@320-8080.ru"
                   :type "text" :subtype "html")
  :test (constantly t)
  :documentation "Template for email tech support about double products warnings during DTD")

(defun email.valid-email-p (email)
  "Ensure that there is an @ and a . and email not containing @s before and after each."
  ;; TODO: make proper validation
  (declare ((or string list) email))
  (every #'(lambda (addr)
             (and (valid-string-p addr) (cl-ppcre:scan "^[^@]+@[^@]+\\.[^@]+$" addr)))
         email))

(defun email.send-xls-doubles-warn (number body)
  "Sends email with warning about double products in xls processing"
  (declare (fixnum number) (string body))
  (when (plusp number)
    (sendmail:send-email-with-template
     +email-warn-template+
     :to +xls-warn-emails+
     :subject (format nil "Doubles in xls: ~D" number)
     :body body)))


(alexandria:define-constant +clientmail-template+
    (make-instance 'sendmail:email
                   :from "shop@320-8080.ru"
                   :type "text" :subtype "html")
  :test (constantly t)
  :documentation "Template for email to client about his/her order")

(defun email.send-client-mail (to order-id body)
  (when (email.valid-email-p to)
    (sendmail:send-email-with-template
     +clientmail-template+
     :to to
     :subject (format nil "Subject: www.320-8080.ru - 3AKA3 ~D~%" order-id)
     :body body)))

(alexandria:define-constant +order-emails+
    (list "internetorder@alpha-pc.com"
          "shop@320-8080.ru"
          "zakaz320@yandex.ru"
          "slamly@gmail.com"
          "wolforus@gmail.com")
  :test (constantly t)
  :documentation "List of emails for sending order details to.")

(alexandria:define-constant +order-details-mail-template+
    (make-instance 'sendmail:email
                   :from "shop@320-8080.ru"
                   :type "text" :subtype "html"
                   ;; FIXME: change to +order-emails+ when deploy
                   :to "toshaevil@gmail.com") ;+order-emails+)
  :test (constantly t)
  :documentation "Template for email to all interested about orders' details")

(defun email.send-order-details (order-id body attachment-filename attachment-string)
  (declare (fixnum order-id) (string body attachment-filename attachment-string))
  (sendmail:send-email-with-template
   +order-details-mail-template+
   :subject (format nil "www.320-8080.ru - 3AKA3 ~D" order-id)
   :body body
   :attachments (sendmail:make-attachment-mime-from-string
                 attachment-string attachment-filename
                 :content-type "text"
                 :content-subtype "plain")))

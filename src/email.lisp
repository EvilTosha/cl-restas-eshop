;;;; email.lisp

(in-package #:eshop)

(alexandria:define-constant +email-warn-template+
    (make-instance 'sendmail:email
                   :from "shop@320-8080.ru"
                   :type "text" :subtype "html")
  :test (constantly t)
  :documentation "Template for email tech support about double products warnings during DTD")

(defun email.valid-email-p (email)
  "Ensure that there is an @ and a . and email not containing @s before and after each."
  (declare ((or string list) email))
  (every #'(lambda (addr)
             (handler-case
                 (data-sift:sift 'data-sift:email addr)
               (data-sift:validation-fail () nil)))
         (ensure-list email)))

(defun email.send-xls-doubles-warn (number body)
  "Sends email with warning about double products in xls processing"
  (declare (fixnum number) (string body))
  (when (plusp number)
    (sendmail:send-email-with-template
     +email-warn-template+
     :to (config.get-option :critical :xls-warn-emails)
     :subject (format nil "Doubles in xls: ~D" number)
     :body body)))


(alexandria:define-constant +clientmail-template+
    (make-instance 'sendmail:email
                   :from "shop@320-8080.ru"
                   :type "text" :subtype "html")
  :test (constantly t)
  :documentation "Template for email to client about his/her order")

(defun email.send-client-mail (to order-id body)
  (declare (fixnum order-id) (string body) ((or list string) to))
  (when (email.valid-email-p to)
    (sendmail:send-email-with-template
     +clientmail-template+
     :to to
     :subject (format nil "www.320-8080.ru - 3AKA3 ~D" order-id)
     :body body)))

(alexandria:define-constant +order-details-mail-template+
    (make-instance 'sendmail:email
                   :from "shop@320-8080.ru"
                   :type "text" :subtype "html"
                   :to (config.get-option :critical :order-emails))
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

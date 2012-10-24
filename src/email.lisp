;;;; email.lisp

(in-package #:eshop)

;; Список email для рассылки писем от ошибках выгрузки 1с
(defvar *conf.emails.gateway.warn* (list "Supplers@alpha-pc.com"
                                         "web_design@alpha-pc.com"
                                         "wolforus@gmail.com"
                                         "slamly@gmail.com"))

(alexandria:define-constant +xls-warn-emails+ (list "wolforus@gmail.com"
                                                    "web_design@alpha-pc.com")
  :test (constantly t)
  :documentation "List of emails to which warnings about double products will be sent")

;; Список email для заказов
(defvar *conf.emails.cart* (list "internetorder@alpha-pc.com"
                                 "shop@320-8080.ru"
                                 "zakaz320@yandex.ru"
                                 "slamly@gmail.com"
                                 "wolforus@gmail.com"))

(alexandria:define-constant +clientmail-template+
    (make-instance 'sendmail:email
                   :from "shop@320-8080.ru"
                   :type "text" :subtype "html")
  :test (constantly t)
  :documentation "Template for email to client about his/her order")

(alexandria:define-constant +email-warn-template+
    (make-instance 'sendmail:email
                   :from "shop@320-8080.ru"
                   :type "text" :subtype "html")
  :test (constantly t)
  :documentation "Template for email tech support about double products warnings during DTD")

(defun email.valid-email-p (email)
  "Ensure that there is an @ and a . and email not containing @s before and after each."
  ;; TODO: make proper validation
  (declare (string email))
  (and (valid-string-p email) (cl-ppcre:scan "^[^@]+@[^@]+\\.[^@]+$" email)))

(defun email.send-xls-doubles-warn (number body)
  "Sends email with warning about double products in xls processing"
  (declare (fixnum number) (string body))
  (when (plusp number)
    (sendmail:send-email-with-template
     +email-warn-template+
     :to +xls-warn-emails+
     :subject (format nil "Doubles in xls: ~D" number)
     :body body)))

(defun email.send-client-mail (to order-id body)
  (when (email.valid-email-p to)
    (sendmail:send-email-with-template
     +clientmail-template+
     :to to
     :subject (format nil "Subject: www.320-8080.ru - 3AKA3 ~D~%" order-id)
     :body body)))


;;; ---------------- old code ----------------


(defun send-mail (to clientmail filename mailfile order-id)
  (let* ((sendmail-process (sb-ext:run-program *sendmail*
                                               to
                                               :input :stream
                                               :output nil
                                               :error nil
                                               :wait nil))
         (sendmail (sb-ext:process-input sendmail-process)))
    (unwind-protect
         (progn
           (format sendmail "From: shop@320-8080.ru~%")
           (format sendmail "To: ~a~%" (car to))
           (format sendmail "Subject: ~a~a~%" "www.320-8080.ru - 3AKA3 " order-id)
           (format sendmail "MIME-Version: ~a~%" "1.0")
           (format sendmail "Content-Type: ~a~%" "multipart/mixed; boundary = becd713b5f8316a655d07bd225b48c406")
           (format sendmail "%")
           (format sendmail
                   "This is a MIME encoded message.

--becd713b5f8316a655d07bd225b48c406
Content-Type: text/html; charset=windows-1251
Content-Transfer-Encoding: base64

~a

--becd713b5f8316a655d07bd225b48c406
Content-Type: Content-type: text/plain; charset=\"windows-1251\"; name = \"~a\"
Content-Transfer-Encoding: base64

~a

--becd713b5f8316a655d07bd225b48c406--
"
                   (encode64 clientmail)
                   filename
                   (encode64 (encode1251 mailfile))))
      (close sendmail)
      (sb-ext:process-wait sendmail-process)
      (sb-ext:process-close sendmail-process))))



(defun encode64 (param)
  (base64:usb8-array-to-base64-string
   ;; (babel:string-to-octets
   ;;  param
   ;;  :encoding :cp1251)))
   (sb-ext:string-to-octets param  :external-format :cp1251) :columns 76))

(defun encode1251 (param)
  (let (($ret nil))
    (loop
       for x across param collect x
       do (if (equal x (code-char 10))
              (progn
                (push (code-char 13) $ret)
                (push x $ret))
              (push x $ret)))
    (coerce (reverse $ret) 'string)))

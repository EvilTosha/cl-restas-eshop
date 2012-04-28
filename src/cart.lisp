;;;; cart.lisp

(in-package #:eshop)

(defvar *order-id* nil)

;;генерирует псевдоуникальный номер заказа
(defun get-order-id ()
  (let ((current-order-id *order-id*)
        (order-id-pathname (format nil "~a~a" (config.get-option "CRITICAL" "path-to-conf") *path-order-id-file*)))
    (if (not (null *order-id*))
        (progn
          (incf *order-id*)
          (with-open-file (file order-id-pathname
                                :direction :output
                                :if-exists :supersede
                                :external-format :utf-8)
            (format file "~a" *order-id*))
          current-order-id)
        (progn
          ;;если в файле шлак, то сбрасываем счетчик заказов до 1
          (setf *order-id*
                (handler-case
                    (parse-integer
                     (alexandria:read-file-into-string
                      order-id-pathname))
                  (SB-INT:SIMPLE-PARSE-ERROR () 1)
                  (SB-INT:SIMPLE-FILE-ERROR () 1)))
          (get-order-id)))))

(defun cart-processor (alist)
  (loop :for item :in alist :collect (item-processor item)))

(defun item-processor (item)
  (let* ((articul   (parse-integer (cdr (assoc :ID item)) :junk-allowed t))
         (group-id  (cdr (assoc :GROUP--ID item)))
         (name      (cdr (assoc :NAME item)))
         (price     (cdr (assoc :PRICE item)))
         (count     (cdr (assoc :COUNT item)))
         (item-link (cdr (assoc :ITEM--LINK item)))
         (img-link  (cdr (assoc :IMG--LINK item)))
         (object    (gethash (format nil "~a" articul) *storage*)))
    (when object
      (let ((pics (get-pics (articul object))))
        (list :count count
              :itemlink item-link
              :firstpic (if (null pics) "" (car pics))
              :articul (articul object)
              :name (realname object)
              :siteprice (siteprice object)
              :price (price object))))))

(defun checkout-page-0 ()
  (if (null (hunchentoot:cookie-in "cart"))
      "null cart"
      (checkout-page (checkout:content0 (list :accessories (soy.product:accessories)
                                              :order (checkout:order))))))

(defun checkout-page-1 ()
  (if (null (hunchentoot:cookie-in "cart"))
      "null cart"
      (checkout-page (checkout:content1 (list :accessories (soy.product:accessories)
                                              :order (checkout:order)
                                              )))))

(defun checkout-page-2 ()
  (if (null (hunchentoot:cookie-in "cart"))
      "null cart"
      (checkout-page (checkout:content2 (list :accessories (soy.product:accessories)
                                                      :order (checkout:order)
                                                      )))))

(defun checkout-page-3 ()
  (if (null (hunchentoot:cookie-in "cart"))
      "null cart"
      (checkout-page (checkout:content3 (list :accessories (soy.product:accessories)
                                                      :order (checkout:order))))))


;;проверка заказа на валидность
;;TODO сделать полную проверку
(defun if-order-valid (products)
  (not (null products)))

(defun save-order-text (file-name body)
	(when (config.get-option "START_OPTIONS" "release")
		(let ((filename (format nil "~a/orders/~a.html" *path-to-dropbox* file-name)))
			(with-open-file
					(stream filename :direction :output :if-exists :supersede)
				(format stream "~a" body)))))


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


(defun send-client-mail (to clientmail order-id)
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

--becd713b5f8316a655d07bd225b48c406--
"
				   (encode64 clientmail)))
      (close sendmail)
      (sb-ext:process-wait sendmail-process)
      (sb-ext:process-close sendmail-process))))



(defun encode64 (param)
  (base64:usb8-array-to-base64-string
   ;; (babel:string-to-octets
   ;;  param
   ;;  :encoding :cp1251)))
   (sb-ext:string-to-octets param  :external-format :cp1251)))

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


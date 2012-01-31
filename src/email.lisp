(in-package #:eshop)

(defvar *email.bin*
  (find-if #'fad:file-exists-p
           (list "/usr/bin/sendmail"
                 "/usr/sbin/sendmail")))

(defvar *sendmail* *email.bin*)

(defvar *email.from* "shop@320-8080.ru")

 (defun email.send-mail-warn (to clientmail error-name)
   (when *email.bin*
     (let* ((sendmail-process (sb-ext:run-program *sendmail*
                                                  to
                                                  :input :stream
                                                  :output nil
                                                  :error nil
                                                  :wait nil))
            (sendmail (sb-ext:process-input sendmail-process)))
       (unwind-protect
            (progn
              (format sendmail "From: ~a~%" *email.from*)
              (format sendmail "To: ~a~%" (car to))
              (format sendmail "Subject: ~a~a~%" "Gateway WARN:" error-name)
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
                      (encode64 clientmail)
                      ))
         (close sendmail)
         (sb-ext:process-wait sendmail-process)
         (sb-ext:process-close sendmail-process)))))




;; (defun send-mail (to head content)
;;   #+sbcl(let* ((sendmail-process (sb-ext:run-program *sendmail*
;;                                                      to
;;                                                      :input :stream
;;                                                      :output nil
;;                                                      :error nil
;;                                                      :wait nil))
;;                (sendmail (sb-ext:process-input sendmail-process)))
;;           (unwind-protect
;;                (progn
;;                  (iter (for head-line in (acons "To" (format nil "~{~A ~}" to)  head))
;;                        (format sendmail
;;                                "~A: ~A~%"
;;                                (car head-line)
;;                                (cdr head-line)))
;;                  (format sendmail "Content-Type: text/html; charset=\"utf-8\"~%~%")
;;                  (typecase content
;;                    (xtree::libxml2-cffi-object-wrapper (xtree:serialize content sendmail))
;;                    (string (write-string content sendmail))
;;                    (pathname (write-string (alexandria:read-file-into-string content) sendmail)))
;;                  t)
;;             (close sendmail)
;;             (sb-ext:process-wait sendmail-process)
;;             (sb-ext:process-close sendmail-process))))

;; (defun prepare-subject (subject &optional (external-format :utf-8))
;;   (format nil
;;           "=?~A?B?~A?="
;;           external-format
;;           (base64:string-to-base64-string
;;            (coerce (loop for code across (string-to-octets subject
;;                                                            :external-format external-format)
;;                       collect (code-char code))
;;                    'string))))


;; (defun send-noreply-mail (receiver subject body &rest bindings)
;;   (send-mail (list receiver)
;;              (acons "From"
;;                     *noreply-mail-account*
;;                     (acons "Subject"
;;                            (prepare-subject subject)
;;                            nil))
;;              (typecase body
;;                (pathname (restas:expand-file body (alexandria:plist-alist bindings)))
;;                (string (restas:expand-text body (alexandria:plist-alist bindings)))
;;                (otherwise (error "bad mail body: ~A" body)))))

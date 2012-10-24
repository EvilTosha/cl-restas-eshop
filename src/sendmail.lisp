;;;; sendmail.lisp

;; depends-on :alexandria

(defpackage :sendmail
  (:use :cl :sb-unix :sb-ext :cl-mime)
  (:export :email
           ;; accessors
           :body
           :to
           :cc
           :bcc
           :subject
           :content-type
           :attachments
           :other-headers
           ;; functions
           :send-email
           :send-email-with-template))


(in-package :sendmail)

(sb-int:defconstant-eqx +sendmail-bin+
    (find-if #'probe-file
             (list "/usr/lib/sendmail"
                   "/usr/bin/sendmail"
                   "/usr/sbin/sendmail"))
  #'string-equal
  "Path to executable sendmail file")

(defclass email ()
  ((from            :initarg :from          :accessor from            :initform nil)
   (to              :initarg :to            :accessor to              :initform nil)
   (reply-to        :initarg :reply-to      :accessor reply-to        :initform nil)
   (subject         :initarg :subject       :accessor subject         :initform "")
   (body            :initarg :body          :accessor body            :initform "")
   (cc              :initarg :cc            :accessor cc              :initform nil)
   (bcc             :initarg :bcc           :accessor bcc             :initform nil)
   (content-type    :initarg :type          :accessor content-type    :initform "text")
   (content-subtype :initarg :subtype       :accessor content-subtype :initform "plain")
   (attachments     :initarg :attachments   :accessor attachments     :initform nil)
   ;; list of strings, one per header
   (other-headers   :initarg :other-headers :accessor other-headers   :initform nil)))


(defmethod print-object ((object email) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "to ~A regarding ~A" (to object) (subject object))))

(defun get-user ()
  (sb-unix:uid-username (sb-unix:unix-getuid)))

(defun send-email (&key (from (get-user)) to reply-to (subject "") (body "") cc bcc (content-type "text")
                   (content-subtype "plain") attachments other-headers)
  "Sends semail with specified parameters"
  (declare (string from subject body content-type content-subtype)
           ((or string list) to reply-to cc bcc)
           (list attachments other-headers)
           (ignore other-headers))
  (let ((sendmail (sb-unix::process-input
                   (sb-ext:run-program +sendmail-bin+
                                       `("-f" ,from
                                              ,@(alexandria:ensure-list to)
                                              ,@(alexandria:ensure-list cc)
                                              ,@(alexandria:ensure-list bcc))
                                       :input :stream
                                       :wait nil)))
        (mime (when attachments
                (make-instance
                 'multipart-mime
                 :subtype "mixed"
                 :content
                 ;; Firstly the text input
                 (cons (make-instance
                        (if (string-equal "text" content-type)
                            'text-mime
                            'mime)
                        :type content-type
                        :subtype content-subtype
                        :content body
                        :disposition "inline")
                       ;; The attachments themselves
                       (mapcar
                        #'(lambda (attachment)
                            (multiple-value-bind (type subtype)
                                (cl-mime:lookup-mime attachment)
                              (make-instance
                               'mime
                               :type (or type "application")
                               :subtype (or subtype "octet-stream")
                               :content (alexandria:read-file-into-byte-vector attachment)
                               :id (cl-mime:make-content-id)
                               :encoding :base64
                               :disposition "attachment"
                               :disposition-parameters
                               `((:filename ,(file-namestring attachment))))))
                        attachments))))))
    (mapc #'(lambda (header value)
              (when value
                (format sendmail "~A: ~{~A~^,~}~%" header (alexandria:ensure-list value))))
          (list "To" "Cc" "From" "Reply-To" "Subject")
          (list  to   cc   from   reply-to   subject))
    (if mime
        (cl-mime:print-mime sendmail mime t t)
        (progn
          (format sendmail "MIME-Version: 1.0~%Content-Type: ~A/~A~%~%"
                  content-type content-subtype)
          (princ body sendmail)))
    (close sendmail)))

(defun send-email-with-template (template-email
                                 &key (from (get-user) from-supplied-p)
                                 (to nil to-supplied-p) (reply-to nil reply-to-supplied-p)
                                 (subject "" subject-supplied-p)
                                 (body "" body-supplied-p)
                                 (cc nil cc-supplied-p) (bcc nil bcc-supplied-p)
                                 (content-type "text" content-type-supplied-p)
                                 (content-subtype "plain" content-subtype-supplied-p)
                                 (attachments nil attachments-supplied-p)
                                 (other-headers nil other-headers-supplied-p))
  (send-email
   :from            (if from-supplied-p            from            (from template-email))
   :to              (if to-supplied-p              to              (to template-email))
   :reply-to        (if reply-to-supplied-p        reply-to        (reply-to template-email))
   :subject         (if subject-supplied-p         subject         (subject template-email))
   :body            (if body-supplied-p            body            (body template-email))
   :cc              (if cc-supplied-p              cc              (cc template-email))
   :bcc             (if bcc-supplied-p             bcc             (bcc template-email))
   :content-type    (if content-type-supplied-p    content-type    (content-type template-email))
   :content-subtype (if content-subtype-supplied-p content-subtype (content-subtype template-email))
   :attachments     (if attachments-supplied-p     attachments     (attachments template-email))
   :other-headers   (if other-headers-supplied-p   other-headers   (other-headers template-email))))



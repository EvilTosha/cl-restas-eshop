;;;; sendmail.lisp

;; depends-on :alexandria :flexi-streams

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
           :send-email-with-template
           :make-attachment-mime-from-file
           :make-attachment-mime-from-string))


(in-package :sendmail)

(alexandria:define-constant +sendmail-bin+
    (find-if #'probe-file
             (list "/usr/lib/sendmail"
                   "/usr/bin/sendmail"
                   "/usr/sbin/sendmail"))
  :test (constantly t)
  :documentation "Path to executable sendmail file")

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


(defun make-attachment-mime-from-file (filepath &key (name (file-namestring filepath))
                            content-type content-subtype)
  "Creates attachment mime object from specified file. If no content-type/content-subtype specified
looks for it in standard mime types table."
  (declare ((or string pathname) filepath) ((or null string) content-type content-subtype))
  (when (probe-file filepath)
    (multiple-value-bind (type subtype)
        (cl-mime:lookup-mime filepath)
      (make-instance
       'mime
       :id (cl-mime:make-content-id)
       :type (or content-type type "application")
       :subtype (or content-subtype subtype "octet-stream")
       :content (alexandria:read-file-into-byte-vector filepath)
       :encoding :base64
       :disposition "attachment"
       :disposition-parameters `((:filename ,name))))))

(defun make-attachment-mime-from-string (string name &key content-type content-subtype)
  "Creates attachment mime object from specified string."
  (declare (string string name) ((or null string) content-type content-subtype))
  (make-instance
   'mime
   :id (cl-mime:make-content-id)
   :type (or content-type "application")
   :subtype (or content-subtype "octet-stream")
   :content (flexi-streams:string-to-octets string :external-format :windows-1251)
   :encoding :base64
   :disposition "attachment"
   :disposition-parameters `((:filename ,name))))


(defun make-body-mime-from-string (string &key content-type content-subtype)
  "Creates attachment mime object from specified string."
  (declare (string string) ((or null string) content-type content-subtype))
  (make-instance
   'mime
   :id (cl-mime:make-content-id)
   :type "text"
   :subtype "html"
   :content (flexi-streams:string-to-octets string :external-format :windows-1251)
   :encoding :base64
   :disposition "inline"))


(defun get-user ()
  (sb-unix:uid-username (sb-unix:unix-getuid)))

(defun send-email (&key (from (get-user)) to reply-to (subject "") (body "") cc bcc (content-type "text")
                   (content-subtype "plain") attachments other-headers)
  "Sends email with specified parameters.
Note: attachments are list of mime objects. To easily create mime from pathname use
#'make-attachment-mime-from-pathname function"
  (declare (string from subject body content-type content-subtype)
           ((or string list) to reply-to cc bcc)
           (list other-headers)
           (ignore other-headers))
  (when +sendmail-bin+
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
                   (cons (make-body-mime-from-string body)
                         ;; The attachments themselves
                         (alexandria:ensure-list attachments))))))
      ;; (error mime)
      (mapc #'(lambda (header value)
                (when value
                  (format sendmail "~A: ~{~A~^,~}~%" header (alexandria:ensure-list value))))
            (list "To" "Cc" "From" "Reply-To" "Subject")
            (list  to   cc   from   reply-to   subject))
      ;; (error mime)
      (if mime
          (cl-mime:print-mime sendmail mime t t)
        (cl-mime:print-mime sendmail (make-body-mime-from-string body) t t))
      (close sendmail))))

(defun send-email-with-template (template-email
                                 &key (from (get-user) from-supplied-p)
                                 (to nil to-supplied-p) (reply-to nil reply-to-supplied-p)
                                 (subject "" subject-supplied-p)
                                 (body "" body-supplied-p)
                                 (cc nil cc-supplied-p) (bcc nil bcc-supplied-p)
                                 (content-type "text" content-type-supplied-p)
                                 (content-subtype "html" content-subtype-supplied-p)
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

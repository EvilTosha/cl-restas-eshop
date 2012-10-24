;;;; sendmail.lisp

;;; Class mail-output-stream.
;;;
;;; FIXME: This information belongs in documentation strings
;;; somewhere. But where? Possibly distributed in various
;;; :documentation slots.
;;;
;;; The mail-output-stream is a class of stream (therefore depending
;;; on the Gray Stream extension) that, on close, sends its
;;; accumulated content by mail.
;;;
;;; Slots:
;;;
;;; * real-stream: contains the stream that the print- functions
;;; actually print to;
;;;
;;; * string-strm: contains the string-strm that is used on
;;; close. This is the same as the real-stream if no stream is
;;; initially supplied; if one is supplied, real-stream is a
;;; broadcast-stream to both the supplied stream and the
;;; string-strm;
;;;
;;; * to, cc, bcc: lists of strings, one per e-mail address;
;;;
;;; * other-headers: A list of strings, one per header.


;; depends-on: alexandria
(defpackage :sendmail
  (:use :cl :sb-gray :sb-unix :sb-ext :sb-bsd-sockets :cl-mime #:cl-base64)
  (:export :mailer-program-error
           :email
           :body
           :to
           :cc
           :bcc
           :subject
           :content-type
           :attachments
           :other-headers))


(in-package :sendmail)

(defparameter *sendmail-bin* "/usr/lib/sendmail")
(defparameter *mail-newline* (coerce (list #\Return #\Newline) 'string))


(defclass email ()
  ((body            :initarg :body          :accessor body            :initform "")
   (subject         :initarg :subject       :accessor subject         :initform "")
   (to              :initarg :to            :accessor to              :initform nil)
   (from            :initarg :from          :accessor from            :initform nil)
   (reply-to        :initarg :reply-to      :accessor reply-to        :initform nil)
   (cc              :initarg :cc            :accessor cc              :initform nil)
   (bcc             :initarg :bcc           :accessor bcc             :initform nil)
   (content-type    :initarg :type          :accessor content-type    :initform "text")
   (content-subtype :initarg :subtype       :accessor content-subtype :initform "plain")
   (attachments     :initarg :attachments   :accessor attachments     :initform nil)
   (other-headers   :initarg :other-headers :accessor other-headers   :initform nil)))


(defmethod print-object ((object email) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "to ~A regarding ~A" (to object) (subject object))))

(defmacro ensure-list (form)
  "If form is list return it, else return (list form)"
  (let ((var (gensym)))
    `(let ((,var ,form))
       (if (listp ,var)
           ,var
           (list ,var)))))

(defmacro ensure-list-setf (form)
  "If form is list, return its value, else return (list form) and evaluate (setf form (list form))"
  `(setf ,form (ensure-list ,form)))

(defun send-email (email)
  (ensure-list-setf (to email))
  (ensure-list-setf (cc email))
  (ensure-list-setf (bcc email))
  (let ((sendmail (sb-unix::process-input
                   (sb-ext:run-program *sendmail-bin*
                                       `("-f" ,(or (from email)
                                                   (sb-unix:uid-username
                                                    (sb-unix:unix-getuid)))
                                              ,@(to email)
                                              ,@(cc email)
                                              ,@(bcc email))
                                       :input :stream
                                       :wait nil)))
        (mime (when (attachments email)
                (make-instance
                 'multipart-mime
                 :subtype "mixed"
                 :content
                 ;; Firstly the text input
                 (cons (make-instance
                        (if (string-equal "text" (content-type email))
                            'text-mime
                            'mime)
                        :type (content-type email)
                        :subtype (content-subtype email)
                        :content (body email)
                        :disposition "inline")
                       ;; The attachments themselves
                       (mapcar
                        #'(lambda (attachment)
                            (multiple-value-bind (type subtype)
                                (cl-mime::lookup-mime attachment)
                              (make-instance
                               'mime
                               :type (or type "application")
                               :subtype (or subtype "octet-stream")
                               :content (alexandria:read-file-into-byte-vector attachment)
                               :id (cl-mime:make-content-id)
                               :encoding :base64
                               :disposition "attachment"
                               :disposition-parameters
                               `((:filename
                                  ,(format nil "~A~@[.~A~]"
                                           (pathname-name
                                            (pathname attachment))
                                           (pathname-type
                                            (pathname attachment))))))))
                        (attachments email)))))))
    (mapc #'(lambda (header value)
              (when value
                (format sendmail "~A: ~{~A~^,~}~%" header (ensure-list value))))
          (list "To" "Cc" "From" "Reply-To" "Subject")
          (list (to email)
                (cc email)
                (from email)
                (reply-to email)
                (subject email)))
    (if mime
        (cl-mime::print-mime sendmail mime t t)
        (progn
          (format sendmail "MIME-Version: 1.0~%Content-Type: ~A/~A~%~%"
                  (content-type email)
                  (content-subtype email))
          (princ (body email) sendmail)))
    (close sendmail)))



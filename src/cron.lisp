;;;; cron.lisp

(in-package #:eshop)

;;; Methods for interacting with cl-cron (http://www.trailoflight.net/cl-cron/)
;;; and definitions for cron-jobs


;; cron-job format
;; (defclass cron-job ()
;;   ((minute :accessor job-minute :initarg :job-minute :initform :every)
;;    (hour :accessor job-hour :initarg :job-hour :initform :every)
;;    (day-of-month :accessor job-dom :initarg :job-dom :initform :every)
;;    (month :accessor job-month :initarg :job-month :initform :every)
;;    (day-of-week :accessor job-dow :initarg :job-dow :initform :every)
;;    (at-boot :accessor job-@boot :initarg :job-@boot :initform nil)
;;    (function-symbol :accessor job-func :initarg :job-func)))

(defun cron.aesthetic-print-job (job)
  (let ((minutes (cl-cron::job-minute job))
        (hours (cl-cron::job-hour job))
        (dom (cl-cron::job-dom job))
        (month (cl-cron::job-month job))
        (dow (cl-cron::job-dow job))
        (func (cl-cron::job-func job)))
    (format nil "Minutes: ~a~&Hours: ~a~&Day of month: ~a~&Month: ~a~&Day-of-week: ~a~&Function: ~a~&"
            (if (eql (length minutes) 60) "every" minutes)
            (if (eql (length hours) 24) "every" hours)
            (if (eql (length dom) 31) "every" dom)
            (if (eql (length month) 12) "every" month)
            (if (eql (length dow) 7) "every" dow)
            func)))

(defun cron.aesthetic-print-job-list ()
  (let ((result ""))
    (maphash (lambda (key value)
               (setf result (format nil "~a~a" result
                                    (format nil "Job key: ~a~&~a~&-----~&" key
                                            (cron.aesthetic-print-job value)))))
             cl-cron::*cron-jobs-hash*)
    result))


(defun cron.html-print-job-list (&optional output)
  (let ((job-list ()))
    (maphash (lambda (key value)
               (let ((minutes (cl-cron::job-minute value))
                     (hours (cl-cron::job-hour value))
                     (dom (cl-cron::job-dom value))
                     (month (cl-cron::job-month value))
                     (dow (cl-cron::job-dow value))
                     (func (cl-cron::job-func value)))
                 (push (list :key (format nil "~a" key)
                             :minutes (if (eql (length minutes) 60) "every" (format nil "~a" minutes))
                             :hours (if (eql (length hours) 24) "every" (format nil "~a" hours))
                             :dom (if (eql (length dom) 31) "every" (format nil "~a" dom))
                             :month (if (eql (length month) 12) "every" (format nil "~a" month))
                             :dow (if (eql (length dow) 7) "every" (format nil "~a" dow))
                             :func (format nil "~a" func))
                       job-list)))
             cl-cron::*cron-jobs-hash*)
    (soy.admin:cron-job-list (list :output output :jobs job-list))))

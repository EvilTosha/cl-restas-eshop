;;;; debug.lisp

;;;; methods for debug (primarily in slime)

(in-package #:eshop)

(defun debug-slime-format (control-string &rest format-arguments)
  "Similar to format, but prints to all current connections' REPLs"
  (mapcar #'(lambda (conn)
              (apply #'format (swank::connection.user-output conn)
                     (format nil "~A~~%" control-string) format-arguments))
          swank::*connections*)
  (values))

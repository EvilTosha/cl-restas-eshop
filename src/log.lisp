;;;; log.lisp

(in-package #:eshop)
(log5:defcategory test)
(log5:defcategory request-log)
(log5:defcategory debug-console)
(log5:defcategory debug-file)
(log5:defcategory info)          ;; console + file
(log5:defcategory info-console)  ;; only for console, not file
(log5:defcategory warning)
(log5:defcategory error)
(log5:defcategory warn+ (or error warning))

(log5:defoutput human-time (time.get-full-human-time))

(defvar *message-lock* (bt:make-lock "message-lock"))

(log5:start-sender 'general-file
                   (log5:stream-sender :location (merge-pathnames "general.log" (config.get-option "PATHS" "path-to-logs")))
                   :category-spec '(or info warn+)
                   :output-spec '(human-time log5:category log5:message))

(log5:start-sender 'general-console
                   (log5:stream-sender :location *query-io*)
                   :category-spec '(or info info-console warn+)
                   :output-spec '(human-time log5:category log5:message))

(log5:start-sender 'debug-console
                   (log5:stream-sender :location *debug-io*)
                   :category-spec '(or test debug-console)
                   :output-spec '(log5:message))


(log5:start-sender 'debug-file
                   (log5:stream-sender :location (merge-pathnames "debug.log" (config.get-option "PATHS" "path-to-logs")))
                   :category-spec '(or debug-file)
                   :output-spec '(log5:message))

(log5:start-sender 'request-log
                   (log5:stream-sender :location (merge-pathnames "request.log" (config.get-option "PATHS" "path-to-logs")))
                   :category-spec '(or request-log)
                   :output-spec '(human-time log5:category log5:message))

;;example of call
;;(log5:log-for debug-console "First log msg")

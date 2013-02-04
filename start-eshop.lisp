;; загрузка модулей и файлов
(defparameter *path-to-libs* (sb-unix::posix-getenv "LIBS_PATH"))
(defparameter *path-to-eshop* (sb-unix::posix-getenv "ESHOP_PATH"))
(defparameter *path-to-config* (sb-unix::posix-getenv "CONFIG_PATH"))
(defparameter *swank-port* (parse-integer (sb-unix::posix-getenv "SWANK_PORT")))

;; регестрация путей для asdf
(load (merge-pathnames "load.lisp" *path-to-eshop*))
(load.register-libs)

;; load swank libs
(asdf:load-system :swank)
;; для того чтобы загружался esrap
(load (merge-pathnames "slime-archimag/contrib/swank-indentation.lisp" *path-to-libs*))
;; swank server start
(print swank::*application-hints-tables*)
(setq swank:*use-dedicated-output-stream* nil)
(swank:create-server :coding-system "utf-8-unix"
										 :dont-close t
										 :port *swank-port*)


;; load eshop
(push *path-to-eshop* asdf:*central-registry*)
(asdf:load-system :eshop)

;; alternative order numbering for developers server
(if (and (not (eshop:config.get-option :start-options :release))
				 (eshop:config.get-option :start-options :dbg-on))
    ;; нумерация заказов
    (setf eshop::*order-id* 1))

(if (eshop:config.get-option :start-options :dbg-on)
		(restas:debug-mode-on)
		(restas:debug-mode-off))
(setf hunchentoot:*catch-errors-p* (eshop:config.get-option :start-options :catch-errors))

(let ((*package* (find-package :eshop)))
	;;; content
  (when (eshop:config.get-option :start-options :load-storage)
    (eshop:sklonenie.restore)
		(eshop:class-core.unserialize-all)
		(eshop:gateway.load))
	(when (eshop:config.get-option :start-options :load-xls)
		(eshop:xls.update-options-from-xls)
    (eshop:cartrige.restore))
	(when (eshop:config.get-option :start-options :load-content)
		(eshop:static-pages.restore)
		(eshop:articles.restore)
		(eshop:main-page.restore))
	(when (eshop:config.get-option :start-options :run-cron-jobs)
		;; making timer for backups
		(cl-cron:make-cron-job #'eshop::backup.serialize-all :minute 0 :hour 17)
		(cl-cron:start-cron))
  ;;; business logic
  (eshop::filters.create-standard-filters)
  (when (eshop:config.get-option :start-options :make-marketing-filters)
    (eshop::groupd.restore)
    (eshop::groupd.holiday.restore)
    (eshop::marketing-filters.create-all-filters)))

;; csv parameters
(setf cl-csv:*newline* (string #\Newline)
      cl-csv:*separator* #\;)

(print (format nil "ESHOP load finished. Time : ~A" (eshop::time.msecs-to-hms (get-internal-real-time))))
(print "Server info: ")
(room)

;; запуск Restas
(restas:start '#:eshop :port (eshop:config.get-option :start-options :server-port))

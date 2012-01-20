;; загрузка модулей и файлов
(defvar *path-to-eshop-home* "eshop")
(defvar *swank-port* 4444)
(defvar *server-port* 4244)

;; регестрация путей для asdf
(load (format nil "~a~a/~a" (user-homedir-pathname) *path-to-eshop-home* "load.lisp"))
(load.register-libs *path-to-eshop-home*)

;; старт сервера swank
(asdf:load-system :swank)
;; для того чтобы загружался esrap
(load (format nil "~a~a/~a" (user-homedir-pathname) *path-to-eshop-home* #P"libs/slime-archimag/contrib/swank-indentation.lisp"))

(print swank::*application-hints-tables*)
(setq swank:*use-dedicated-output-stream* nil)
(swank:create-server :coding-system "utf-8-unix" :dont-close t :port *swank-port*)

(asdf:load-system :eshop)

;; запуск Restas
(restas:start '#:eshop :port *server-port*)
(restas:debug-mode-off)
(setf hunchentoot:*catch-errors-p* t)

(eshop::restore-skls-from-files)
(eshop::articles.restore)
(eshop::main-page.restore)
(let ((*package* (find-package :eshop)))
 (eshop::new-classes.unserialize-all))
(eshop::static-pages.restore)
(eshop::gateway.restore-history)
(eshop::report.set-salefilter)
(eshop::dtd)
(eshop::groupd.restore)

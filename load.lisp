(require 'asdf)

(defun cl-eshop-path (name &rest filenames)
  (pathname (format nil "~{~a~^/~}" (append (list (user-homedir-pathname) name) filenames))))

(defun cl-eshop-push-asdf (home filename)
  (push (truename (cl-eshop-path home "libs" filename)) asdf:*central-registry*))

(defparameter cl-eshop-libs
  (list
   ;; "slime-2011-09-01" ;; актуальный swank
   "slime-archimag" ;; SWANK
   "alexandria"     ;; hunch
   "bordeaux-threads" ;; hunch
   "usocket-0.5.2" ;; hunch
   "md5-1.8.5"     ;; hunch
   "rfc2388" ;; hunch
   "flexi-streams-1.0.7" ;; hunch
   "cl-base64-3.3.3" ;; hunch
   "cl-fad-0.6.4" ;; hunch
   "trivial-garbage_0.19" ;; hunch
   "cl-who-0.11.1" ;; hunch
   "cl-ppcre-2.0.3" ;; hunch
   "chunga-1.1.1" ;; hunch
   "trivial-gray-streams-2008-11-02" ;;hunch
   "trivial-backtrace" ;; hunch
   "cffi" ;; hunch
   "trivial-features_0.6" ;; hunch
   "babel_0.3.0"   ;; hunch
   "cl-plus-ssl"   ;; hunch
   "hunchentoot"   ;; HUNCH
   "closer-mop" ;;restas
   "data-sift" ;;restas
   "cl-puri-1.5.5" ;;restas
   "parse-number-1.2" ;;restas
   "cl-routes-0.2.5" ;;restas
   "split-sequence-1.0" ;;restas
   "iterate-1.4.3" ;;restas
   "restas" ;; RESTAS
   "cl-closure-template" ;; шаблонизатор
   "cl-json_0.4.1" ;; JSON сериализатор
   "arnesi_dev-20080427" ;; parse-float
   "parenscript" ;;closure-template
   "named-readtables-0.9" ;;closure-template
   "anaphora-0.9.4" ;; closure-template | macro collection from Hell http://www.cliki.net/Anaphora
   "esrap" ;; closure-template | packrat parser http://nikodemus.github.com/esrap/
   "log5" ;; логирование | logging framework http://common-lisp.net/project/log5/
   ;; "cl-store" ;; сохранение данных | Serialization Package http://common-lisp.net/project/cl-store/
   "drakma-1.2.4" ;;http клиент для тестов
	 "cl-cron" ;; http://www.trailoflight.net/cl-cron/
	 ))


;; "cl-restas-eshop"
(defun load.register-libs (name)
  (push (truename (pathname (format nil "~a~a" (user-homedir-pathname) name))) asdf:*central-registry*)
  ;; регистрация дерикторий библиотек
  (mapcar #'(lambda (v) (cl-eshop-push-asdf name v))
                    cl-eshop-libs))


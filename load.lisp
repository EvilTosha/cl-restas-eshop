(require 'asdf)

(defun load.register-libs ()
	(mapcar #'(lambda (lib-name)
							(push (truename (merge-pathnames lib-name *path-to-libs*)) asdf:*central-registry*))
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
					 "split-sequence-1.0" ;;restas; required for namespaces in templates
					 "iterate-1.4.3" ;;restas
					 "restas" ;; RESTAS
					 "cl-closure-template" ;; шаблонизатор
					 "cl-json_0.4.1" ;; JSON сериализатор
					 "arnesi_dev-20080427" ;; parse-float
					 "parenscript-2.5" ;;closure-template
					 "named-readtables-0.9" ;;closure-template
					 "anaphora-0.9.4" ;; closure-template | macro collection from Hell http://www.cliki.net/Anaphora
					 "esrap" ;; closure-template | packrat parser http://nikodemus.github.com/esrap/
					 "log5" ;; логирование | logging framework http://common-lisp.net/project/log5/
					 ;; "cl-store" ;; сохранение данных | Serialization Package http://common-lisp.net/project/cl-store/
					 "drakma-1.2.8" ;;http клиент для тестов
					 ;; "cl-cron" ;; http://www.trailoflight.net/cl-cron/
					 "py-configparser-1.0.3" ;;
           "string-case"
           "cl-csv"
           "cl-interpol-0.2.1"
           "cl-unicode-0.1.4"
           "cl-mime"
           "cl-qprint"
           ;;mongoDB
           "cl-mongo"
           "lisp-unit" ;; mongo
           "documentation-template-0.4.2" ;; mongo
           "uuid" ;; mongo
           "trivial-utf-8" ;; mongo
           "ironclad" ;; mongo
           "nibbles" ;; mongo
           ;; "js" ;; test parenscript
           ;; "parse-js" ;; -> js
           ;; "Eos"
           )))



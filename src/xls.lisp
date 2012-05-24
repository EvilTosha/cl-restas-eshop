;;;; xls.lisp

(in-package #:eshop)

(defvar *xls.product-table* (make-hash-table :test #'equal))
(defvar *xls.errors* nil)
(defvar *xls.errors-num* 0)


(defclass nko ()
  ((folder  :initarg :folder  :initform nil :accessor folder)
   (xls2csv :initarg :xls2csv :initform nil :accessor xls2csv)))


(defmethod initialize-instance :after ((obn nko) &key)
  (unless (file-exists-p (xls2csv obn))
    (error "xls2csv not found"))
  (unless (directory-exists-p (folder obn))
    (error "folder not found")))

(defparameter px (make-instance 'nko
                                :folder  (format nil "~aDropbox/xls" (user-homedir-pathname))
                                :xls2csv "/usr/bin/xls2csv"))

(defmethod ƒ ((isg string) (obn nko))
  (let ((bin))
    (values
     (mapcar #'(lambda (y) (string-trim '(#\Space #\Tab) y))
             (mapcar #'(lambda (y) (regex-replace-all "\\s+" y " "))
                     (mapcar #'(lambda (y) (string-trim '(#\Space #\Tab #\") y))
                             (let ((inp) (sv) (ac) (rs))
                               (loop :for cr :across isg do
                                     (if (null inp)
                                         (cond ((equal #\" cr) (setf inp t))
                                               ((equal #\, cr) (push "" rs)))
                                       (cond ((and (null sv) (equal #\" cr)) (setf sv t))
                                             ((and sv (equal #\" cr)) (progn (setf sv nil)
                                                                             (push #\" ac)))
                                             ((and sv (equal #\, cr)) (progn (setf sv nil)
                                                                             (setf inp nil)
                                                                             (push (coerce (reverse ac) 'string) rs)
                                                                             (setf ac nil)))
                                             ((equal #\Return cr) nil)
                                             (t (push cr ac)))))
                               (when ac
                                 (if (and inp (null sv))
                                     (setf bin t))
                                 (push (coerce (reverse ac) 'string) rs))
                               (reverse rs)))))
     bin)))


(defmethod ƒ ((prm list) (obn nko))
  (let* ((line (getf prm :line))
         (optgroups (getf prm :optgroups))
         (fields (getf prm :fields))
         (flt)
         (rs)
         (mx (max (length line) (length optgroups) (length fields)))
         (cur-optgroup)
         (cur-options))
    (loop :for i :from 0 :to (- mx 1) :do
          (let ((val       (if (nth i line)       (nth i line) ""))
                (optgroup  (if (nth i optgroups)  (nth i optgroups) ""))
                (field     (if (nth i fields)     (nth i fields) "")))
            (cond ((equal i 0) (setf (getf flt :articul)
                                     (parse-integer val)))
                  ((equal i 1) (setf (getf flt :realname) val))
                  (t (progn (unless (equal 0 (length optgroup))
                              (unless (null cur-optgroup)
                                (push (list :optgroup_name cur-optgroup :options (reverse cur-options)) rs))
                              (setf cur-optgroup optgroup)
                              (setf cur-options nil))
                            (push (list :name field :value val) cur-options))))))
    (push (list :optgroup_name cur-optgroup :options (reverse cur-options)) rs)
    (append flt (list :result-options (reverse rs)))))


(defmethod ƒ ((ifl pathname) (obn nko))
  (let ((rs)
        (otp)
        (log-output *standard-output*))
    (setf otp (with-output-to-string (*standard-output*)
                                     (let* ((proc (sb-ext:run-program
                                                   (xls2csv obn)
                                                   (list "-q3" (format nil "~a" ifl)) :wait nil :output :stream))
                                            (optgroups)
                                            (fields))
                                       (with-open-stream (in (sb-ext:process-output proc))
                                                         (loop
                                                          :for ist = (read-line in nil)
                                                          :until (or (null ist)
                                                                     (string= "" (string-trim "#\," ist)))
                                                          :do (progn
                                                                (multiple-value-bind (line esf)
                                                                    (ƒ ist px)
                                                                  (when esf
                                                                    (format log-output "~&~a|~a:~a" ifl line esf)
                                                                    (error "DTD"))
                                                                  (unless (null line)
                                                                    (cond ((null optgroups) (setf optgroups line))
                                                                          ((null fields) (setf fields line))
                                                                          (t (handler-case
                                                                              (let ((val (ƒ (list :line line
                                                                                                  :optgroups optgroups
                                                                                                  :fields fields)
                                                                                            px)))
                                                                                (print "")
                                                                                (print val)
                                                                                (push val rs))
                                                                              (SB-INT:SIMPLE-PARSE-ERROR () nil))))))))))))
    rs))

(defmethod xls.process-all-dtd ((jct nko) (obn nko))
  (log5:log-for info "Processing DTD...")
  (let ((cnt 0)
        (items nil)
        (num-all 0))
    (loop :for file :in (directory (format nil "~a/*.xls" (folder obn))) :do
          (setf items (reverse (ƒ file px)))
          (setf num-all (+ num-all (length items)))
          (log5:log-for info-console "~a. Processing file: ~a | ~a" (incf cnt) file (length items))
          (loop :for item :in items :do
                (let* ((articul (getf item :articul))
                       (realname (getf item :realname))
                       (optgroups (loop :for optgroup :in (getf item :result-options) :collect
                                        (list :name (getf optgroup :optgroup_name)
                                              :options (loop :for option :in (getf optgroup :options) :collect
                                                             (list  :name (getf option :name)
                                                                    :value (getf option :value))))))
                       (product (gethash (format nil "~a" articul) (storage *global-storage*))))
                  (let ((pr (gethash articul *xls.product-table*)))
                    (if pr
                        (progn
                          (log5:log-for warning "WARN:~a | ~a | ~a" articul pr file)
                          (setf *xls.errors* (concatenate 'string (format nil "<tr><td>~a</td>
                                                                         <td><a href=\"http://320-8080.ru/~a\">~a</a></td>
                                                                         <td>~a</td>
                                                                         <td>~a</td></tr>" articul articul realname
                                                                         (car (last (split-sequence:split-sequence #\/ (format nil "~a" pr))))
                                                                         (car (last (split-sequence:split-sequence #\/ (format nil "~a" file))))) *xls.errors*))
                          (setf *xls.errors-num* (+ *xls.errors-num* 1)))
                      (setf (gethash articul *xls.product-table*) file)))
                  (if (null product)
                      (format nil "warn: product ~a (articul ~a) not found, ignore (file: ~a)" realname articul file)
                    (progn
                      (setf (optgroups product) optgroups)
                      (with-option1 product "Общие характеристики" "Производитель"
                                    (setf (vendor product) (getf option :value)))
                      ;; Если есть значимое realname - перезаписать в продукте
                      (if (not (string= "" (string-trim '(#\Space #\Tab #\Newline)
                                                        (format nil "~@[~a~]" realname))))
                          (setf (name-seo product) realname)))))))
    (log5:log-for info "Successfully processed ~a files | ~a products" cnt num-all)))


(defun dtd ()
  (let ((*xls.errors* "<table>")
        (*xls.errors-num* 0))
    (setf *xls.product-table* (make-hash-table :test #'equal))
    (xls.process-all-dtd px px)
    (setf *xls.errors* (concatenate 'string "</table>" *xls.errors*))
    (mapcar #'(lambda (email)
                (email.send-mail-warn (list email) *xls.errors* (format nil "дубли в dtd: ~a" *xls.errors-num*)))
            *conf.emails.xls.warn*)))

(defun xls.restore-from-xls (filepath line-processor &optional (restore-name "restore-from-xls"))
  (log5:log-for info "Start ~a from file ~a" restore-name filepath)
  (let* ((file (format nil "~a" filepath))
         (proc (when (file-exists-p file)
                 (sb-ext:run-program
                  "/usr/bin/xls2csv"
                  (list "-q3" file) :wait nil :output :stream))))
    (when proc
      (with-open-stream (stream (sb-ext:process-output proc))
                        (read-line stream nil)
                        (loop
                         :for line = (read-line stream nil)
                         :while (servo.valid-string-p line :unwanted-chars (list #\,) :del-method :trim)
                         :do (funcall line-processor line))))
    (log5:log-for info "DONE ~a" restore-name)))

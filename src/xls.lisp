;;;; xls.lisp

(in-package #:eshop)

(defvar *xls.product-table* (make-hash-table :test #'equal))
(defvar *xls.errors* nil)
(defvar *xls.errors-num* 0)


(alexandria:define-constant +xls2csv-bin+
    (find-if #'probe-file
             (list "/usr/bin/xls2csv"
                   "/usr/lib/xls2csv"
                   "/usr/sbin/xls2csv"))
  :test (constantly t)
  :documentation "Path to executable xls2csv file")

(defmethod ƒ ((isg string))
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


(defmethod ƒ ((prm list))
  (let* ((line (getf prm :line))
         (optgroups (getf prm :optgroups))
         (fields (getf prm :fields))
         (flt)
         (rs)
         (mx (max (length line) (length optgroups) (length fields)))
         (cur-optgroup)
         (cur-options))
    (loop :for i :from 0 :to (1- mx) :do
       (let ((val       (if (nth i line)       (nth i line) ""))
             (optgroup  (if (nth i optgroups)  (nth i optgroups) ""))
             (field     (if (nth i fields)     (nth i fields) "")))
         (cond ((zerop i) (setf (getf flt :articul)
                                (parse-integer val)))
               ((equal i 1) (setf (getf flt :realname) val))
               (t (progn (unless (zerop (length optgroup))
                           (unless (null cur-optgroup)
                             (push (list :optgroup_name cur-optgroup :options (reverse cur-options)) rs))
                           (setf cur-optgroup optgroup)
                           (setf cur-options nil))
                         (push (list :name field :value val) cur-options))))))
    (push (list :optgroup_name cur-optgroup :options (reverse cur-options)) rs)
    (append flt (list :result-options (reverse rs)))))


(defmethod ƒ ((ifl pathname))
  (let ((rs)
        (otp)
        (log-output *standard-output*))
    (setf otp (with-output-to-string (*standard-output*)
                (let* ((proc (sb-ext:run-program
                              +xls2csv-bin+
                              (list "-q3" (format nil "~a" ifl)) :wait nil :output :stream))
                       (optgroups)
                       (fields))
                  (with-open-stream (in (sb-ext:process-output proc))
                    (loop
                       :for ist := (read-line in nil)
                       :while (valid-string-p ist :unwanted-chars '(#\,))
                       ;; (or (null ist)
                                  ;; (string= "" (string-trim "#\," ist)))
                       :do (multiple-value-bind (line esf)
                               (ƒ ist)
                             (when esf
                               (format log-output "~&~a|~a:~a" ifl line esf)
                               (error "DTD"))
                             (when line
                               (cond ((null optgroups) (setf optgroups line))
                                     ((null fields) (setf fields line))
                                     (t (handler-case
                                            (let ((val (ƒ (list :line line
                                                                :optgroups optgroups
                                                                :fields fields))))
                                              (print val)
                                              (push val rs))
                                          (SB-INT:SIMPLE-PARSE-ERROR () nil)))))))))))
    rs))

(defmethod xls.process-all-dtd ()
  (log5:log-for info "Processing DTD...")
  (let ((cnt 0)
        (items nil)
        (num-all 0))
    (loop :for file :in (remove-if #'(lambda (file) (or (directory-pathname-p file)
                                                        (not (equal (pathname-type file) "xls"))))
                                   (rename-recursive-get-files (config.get-option :paths :path-to-xls)))
       :do
       (setf items (reverse (ƒ file)))
       (incf num-all (length items))
       (log5:log-for info-console "~a. Processing file: ~a | ~a" (incf cnt) file (length items))
       (loop :for item :in items :do
          (let* ((articul (getf item :articul))
                 (realname (getf item :realname))
                 (optgroups (loop :for optgroup :in (getf item :result-options) :collect
                               (list :name (getf optgroup :optgroup_name)
                                     :options (loop :for option :in (getf optgroup :options) :collect
                                                 (list  :name (getf option :name)
                                                        :value (getf option :value))))))
                 (product (getobj (format nil "~a" articul) 'product))
                 (pr (gethash articul *xls.product-table*)))
            (if pr
                (progn
                  (log5:log-for warning "WARN:~a | ~a | ~a" articul pr file)
                  (setf *xls.errors* (concatenate 'string (format nil "<tr><td>~a</td>
                                                                         <td><a href=\"http://320-8080.ru/~a\">~a</a></td>
                                                                         <td>~a</td>
                                                                         <td>~a</td></tr>" articul articul realname
                                                                         (car (last (split-sequence:split-sequence #\/ (format nil "~a" pr))))
                                                                         (car (last (split-sequence:split-sequence #\/ (format nil "~a" file))))) *xls.errors*))
                  (setf *xls.errors-num* (1+ *xls.errors-num*)))
                ;; else
                (setf (gethash articul *xls.product-table*) file))
            (if (null product)
                (log5:log-for warning
                              "product ~a (articul ~a) not found, ignore (file: ~a)"
                              realname articul file)
                ;; else
                (progn
                  (setf (optgroups product) optgroups)
                  (setf (vendor product)
                        (get-option product "Общие характеристики" "Производитель"))
                  ;; Если есть значимое realname - перезаписать в продукте
                  (when (valid-string-p realname)
                    (setf (name-seo product) realname)))))))
    (log5:log-for info "Successfully processed ~a files | ~a products" cnt num-all)))


(defun dtd ()
  (let ((*xls.errors* "<table>")
        (*xls.errors-num* 0))
    (setf *xls.product-table* (make-hash-table :test #'equal))
    (xls.process-all-dtd)
    (setf *xls.errors* (concatenate 'string "</table>" *xls.errors*))
    (email.send-xls-doubles-warn *xls.errors-num* *xls.errors*)))

(defun xls.restore-from-xls (filepath line-processor &optional (restore-name "restore-from-xls"))
  (log5:log-for info "Start ~a from file ~a" restore-name filepath)
  (let* ((file (format nil "~a" filepath))
         (proc (when (file-exists-p file)
                 (sb-ext:run-program
                  +xls2csv-bin+
                  (list "-q3" file) :wait nil :output :stream))))
    (when proc
      (with-open-stream (stream (sb-ext:process-output proc))
        (read-line stream nil)
        (loop
           :for line := (read-line stream nil)
           :while (valid-string-p line :unwanted-chars (list #\, #\Space #\Tab) :del-method :trim)
           :do (funcall line-processor line))))
    (log5:log-for info "DONE ~a" restore-name)))

(in-package #:closure-template)

(defun make-call-command-hadler (cmd)
  (destructuring-bind (name data &rest params) (cdr cmd)
    (let ((data-expr (make-call-data-handler data))
          (args (iter (for param in params)
                      (collect
                          (cons (second (second param))
                                (make-param-handler param)))))
          (name-expr (if (stringp name)
                         (constantly name)
                         (make-expression-handler name))))
      (named-lambda call-command-handler (env out)
        (let* ((fullname (funcall name-expr env))
               (last-point (search "." fullname :from-end t))
               (name (if last-point
                         (subseq fullname (1+ last-point))
                         fullname))
               (ttable (if last-point
                           (package-ttable (lispify-string (subseq fullname 0 last-point)))
                           *ttable*)))
          (ttable-call-template ttable
                                (lispify-string name)
                                (append (iter (for arg in args)
                                              (collect (car arg))
                                              (collect (funcall (cdr arg) env)))
                                        (funcall data-expr env))
                                out))))))

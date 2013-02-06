;;;; xls.lisp

(in-package #:eshop)

(alexandria:define-constant +xls2csv-bin+
    (find-if #'probe-file
             (list "/usr/bin/xls2csv"
                   "/usr/lib/xls2csv"
                   "/usr/sbin/xls2csv"))
  :test (constantly t)
  :documentation "Path to executable xls2csv file")

(defvar *xls-last-modified* (make-hash-table :test #'equal))

(defun xls.%read-and-process-file (pathname)
  "Processes the xls file and returns a list of options lists"
  (declare (pathname pathname))
  (let (table)
    (with-output-to-string (*standard-output*)
      (let ((proc (sb-ext:run-program
                   +xls2csv-bin+
                   (list "-q3" (namestring (truename pathname))) :wait nil :output :stream)))
        (with-open-stream (stream (sb-ext:process-output proc))
          (setf table (cl-csv:read-csv stream :separator #\,)))))
    table))

(defun xls.%prepare-optgroups (option-descriptor header)
  "By given option descriptor and two header rows returns optgroup in the same format as in product"
  ;; first two columns (articul and name) aren't used
  (loop
     :for optgroup :in (cddr (first header))
     :for option :in (cddr (second header))
     :for value :in (cddr option-descriptor)
     :with optgroups := nil
     :with cur-optgroup := ""
     :with options-collector := nil
     :when (valid-string-p optgroup :whitespace-check nil)
     :do (when (valid-string-p cur-optgroup :whitespace-check nil)
           (push (list :name cur-optgroup
                       :options (reverse options-collector))
                 optgroups))
           (setf options-collector nil
                 cur-optgroup optgroup)
     :do (push (list :name option :value value)
               options-collector)
     :finally (progn
                (push (list :name cur-optgroup
                            :options (reverse options-collector))
                      optgroups)
                (return (reverse optgroups)))))

(defun xls.%prepare-error-email (errors-table)
  (declare (hash-table errors-table))
  (with-output-to-string (stream)
    (format stream "<table>")
    (maphash #'(lambda (key files)
                 (format stream "<tr><td>~A</td>
                                 <td><a href=\"http://320-8080.ru/~A\">~A</a></td>
                                 <td>~{~A ~}</td></tr>"
                         key key key files))
             errors-table)
    (format stream "</table>")))

(defun xls.%update-options-single-file (file articul-file error-articuls)
  (declare ((or pathname string) file) (hash-table articul-file error-articuls))
  (log5:log-for info-console "Processing file: ~A" file)
  (let ((option-table (xls.%read-and-process-file file)))
    (loop
       :for option-descriptor :in (cddr (butlast option-table)) ; without 2 header rows and last crap row
       :do (let* ((key (first option-descriptor))
                  (name (second option-descriptor))
                  (product (getobj key 'product)))
             (when (valid-string-p key :whitespace-check nil)
               (if (null product)
                   (log5:log-for warning
                                 "Product ~A (articul ~A), not found, skip. File: ~A"
                                 name key file)
                   ;; else
                   (progn
                     (when (valid-string-p name :whitespace-check nil)
                       (setf (name-seo product) name))
                     (setf (optgroups product)
                           (xls.%prepare-optgroups option-descriptor (list (first option-table)
                                                                           (second option-table)))
                           (vendor product) (get-option product "Общие характеристики" "Производитель"))))
               (if (gethash key articul-file)
                   (asif (gethash key error-articuls)
                         (push file it)
                         ;; else
                         (setf it (list (gethash key articul-file) file)))
                   ;; else
                   (setf (gethash key articul-file) file)))))))

(defun xls.update-options-from-xls ()
  (let (;; hash-table that stores for each articul the first file where it has occured
        (articul-file (make-hash-table :test #'equal))
        ;; hash-table that for each dubbing articul stores a list of all files where it has occured
        (error-articuls (make-hash-table :test #'equal)))
    (cl-fad:walk-directory
     (config.get-option :paths :path-to-xls)
     #'(lambda (file)
         ;; check last modified date
         (let ((file-last-modified (file-write-date file)))
           (slet (gethash file *xls-last-modified*)
             (when (or (not it)
                       (< it file-last-modified))
               (xls.%update-options-single-file file articul-file error-articuls)
               (setf it file-last-modified)))))
     :test #'(lambda (file)
               (and (not (directory-pathname-p file))
                    (equal (pathname-type file) "xls"))))
    (email.send-xls-doubles-warn (hash-table-count error-articuls)
                                 (xls.%prepare-error-email error-articuls))))



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

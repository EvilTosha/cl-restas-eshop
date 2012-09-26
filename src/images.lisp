;;;; images.lisp

(in-package #:eshop)

(defun get-dimensions (path-to-image)
  (let* ((string
          (let ((proc (sb-ext:run-program "/usr/bin/identify"
                                          (list "-format" "%w %h" (format nil "~A" path-to-image)) :wait nil :output :stream)))
            (with-open-stream (in (sb-ext:process-output proc))
              (read-line in))))
         (dimensions (split-sequence #\Space string :test #'char=)))
    (list :width (parse-integer (first dimensions))
          :height (parse-integer (second dimensions)))))

(defun style-for-resize (width height req-size)
  (if (>= width height)
      (format nil "width:~apx" (min width req-size))
      (when (> height width)
        (format nil "height:~apx" (min height req-size)))))

(defun get-pics (articul)
  (let* ((articul-str (format nil "~a" articul))
         (path-art  (ppcre:regex-replace  "(\\d{1,3})(\\d{0,})"  articul-str  "\\1/\\1\\2" ))
         (path (format nil "~a/big/~a/*.jpg" (config.get-option "PATHS" "path-to-pics") path-art)))
    (loop
       :for pic
       :in (ignore-errors (directory path))
       :collect (format nil "~a.~a"
                        (pathname-name pic)
                        (pathname-type pic)))))

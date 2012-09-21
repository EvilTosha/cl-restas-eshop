;;;; images.lisp

(in-package :pics)

(defun get-dimensions (path-to-image)
  (let* ((string
          (let ((proc (sb-ext:run-program "/usr/bin/identify"
                                          (list "-format" "%w %h" (format nil "~A" path-to-image)) :wait nil :output :stream)))
            (with-open-stream (in (sb-ext:process-output proc))
              (read-line in))))
         (dimensions (split-sequence:split-sequence #\Space string :test #'char=)))
    (list :width (parse-integer (first dimensions))
          :height (parse-integer (second dimensions)))))

(defun style-for-resize (width height req-size)
  (if (>= width height)
      (format nil "width:~apx" (min width req-size))
      (when (> height width)
        (format nil "height:~apx" (min height req-size)))))


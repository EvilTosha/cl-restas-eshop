(in-package #:closure-template.parser)

(define-rule template-name (and alpha-char (*  (or alphanumeric #\_ #\- #\.)))
  (:text t))


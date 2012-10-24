(in-package :mime)

(defclass text-mime (mime)
  ((content-type
    :reader content-type
    :initform "text")
   (content-subtype
    :reader content-subtype
    :initform "plain"
    :initarg :subtype)
   (charset
    :accessor charset
    :initarg :charset
    :initform "utf-8"))
  (:documentation "Text MIME Object Representation"))

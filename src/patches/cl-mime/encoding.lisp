(in-package :mime)

(defconstant +column-size+ 76
  "Size of column, after each CRLF should be inserted. See MIME RFC (http://tools.ietf.org/html/rfc2045) for more details.")

(defun encode-content (mime)
  (if (eql (content-transfer-encoding mime)
           (content-encoding mime))
      (content mime)
      (let ((content (decode-content mime)))
        (ecase (content-transfer-encoding mime)
          (:7bit content)
          (:base64
           (typecase content
             (string (string-to-base64-string content :columns +column-size+))
             ((array (unsigned-byte 8))
              (usb8-array-to-base64-string content :columns +column-size+))))
          (:quoted-printable (qprint:encode content :columns +column-size+))))))

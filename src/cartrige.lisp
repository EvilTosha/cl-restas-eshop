;;;; cartrige.lisp

(in-package #:eshop)

(defclass printer ()
  ((original-cartriges :initarg :original-cartriges :initform nil   :accessor original-cartriges)
   (other-cartriges    :initarg :other-cartriges    :initform nil   :accessor other-cartriges)
   (vendor             :initarg :vendor             :initform ""    :accessor vendor)
   (name               :initarg :name               :initform ""    :accessor name)
   (printer-type       :initarg :printer-type       :initform ""    :accessor printer-type)))

(defparameter *printer-storage* (make-hash-table :test #'equal))

(restas:define-route cartrige-select-route ("/cartriges-select")
  (let* ((get-data (servo.alist-to-plist (hunchentoot:get-parameters hunchentoot:*request*)))
         (vendor (getf get-data :vendor))
         (type (getf get-data :type))
         (printers (cartrige.get-printers-by-params :vendor vendor :printer-type type)))
    (format nil "[~{~a~^,~}]"
            (mapcar #'(lambda (printer)
                        (format nil "{\"name\":\"~a\", \"key\":\"~a\"}" (name (gethash printer *printer-storage*)) printer))
                    printers))))

(restas:define-route cartrige-select-type-route ("/cartriges-select-type")
  (let* ((get-data (servo.alist-to-plist (hunchentoot:get-parameters hunchentoot:*request*)))
         (vendor (getf get-data :vendor)))
    (when vendor
      (format nil "[~{\"~a\"~^,~}]" (cartrige.get-types-by-vendor vendor)))))

(defun cartrige.add-printer (storage articul original-cartriges other-cartriges
                             &optional vendor name printer-type)
  (let ((printer (gethash articul (storage *global-storage*))))
    (when (or original-cartriges
              other-cartriges)
      (setf (gethash articul storage)
            (make-instance 'printer
                           :original-cartriges original-cartriges
                           :other-cartriges other-cartriges
                           :vendor (if vendor
                                       vendor
                                       (vendor printer))
                           :name (if name
                                     name
                                     (name printer))
                           :printer-type printer-type)))))

(defun cartrige.add-cartrige (storage printer-articul cartrige-articul &optional (cartrige-type :other))
  (if (eq cartrige-type :original)
      (pushnew cartrige-articul (original-cartriges (gethash printer-articul storage)))
      (pushnew cartrige-articul (other-cartriges (gethash printer-articul storage)))))


(defun cartrige.get-cartriges-by-model (articul &optional (cartrige-type :all))
  (let ((printer (gethash articul *printer-storage*))
        (global-printer (gethash articul (storage *global-storage*))))
    (when (and printer global-printer)
      (case cartrige-type
        (:all (append (original-cartriges printer) (other-cartriges printer)))
        (:original (original-cartriges printer))
        (:other (other-cartriges printer))))))

(defun cartrige.get-printers-by-params (&key vendor printer-type)
  (let ((result nil))
    (maphash #'(lambda (k v)
                 (when (and (or (null vendor)
                                (string= vendor (vendor v)))
                            (or (null printer-type)
                                (string= printer-type (printer-type v))))
                   (push k result)))
             *printer-storage*)
    (sort result #'(lambda (a1 a2)
                     (string< (name (gethash a1 *printer-storage*))
                              (name (gethash a2 *printer-storage*)))))))

(defun cartrige.get-types-by-vendor (vendor)
  (let ((result nil))
    (maphash #'(lambda (k v)
                 (declare (ignore k))
                 (when (string= vendor (vendor v))
                   (pushnew (printer-type v) result :test #'string=)))
             *printer-storage*)
    (sort result #'string<)))

(defun cartrige.get-vendors-list ()
  (sort (let (result)
          (maphash #'(lambda (k v)
                       (declare (ignore k))
                       (pushnew (vendor v) result :test #'string=))
                   *printer-storage*)
          result)
        #'string<))

(defun cartrige.get-types-list ()
  (sort (let (result)
          (maphash #'(lambda (k v)
                       (declare (ignore k))
                       (pushnew (printer-type v) result :test #'string=))
                   *printer-storage*)
          result)
        #'string<))

(defun cartrige.restore ()
  (let ((t-storage (make-hash-table :test #'equal)))
    (xls.restore-from-xls
     (merge-pathnames "printyrikartridji.xls" (config.get-option "CRITICAL" "path-to-conf"))
     #'(lambda (line)
         (let* ((content-list (mapcar #'(lambda (elt)
                                          (string-trim "#\"" elt))
                                      (split "," line)))
                (vendor (nth 0 content-list))
                (type (nth 1 content-list))
                (tech (nth 2 content-list))
                (name (nth 3 content-list))
                (articul (nth 4 content-list))
                (origin-list (loop
                                :for num
                                :from 5
                                :for elt = (nth num content-list)
                                :while (servo.is-valid-string elt)
                                :collect elt))
                (other-list (loop
                               :for num
                               :from 18
                               :for elt = (nth num content-list)
                               :while (servo.is-valid-string elt)
                               :collect elt)))
           (cartrige.add-printer t-storage articul origin-list other-list vendor name
                                 (format nil "~a ~(~a~)" type tech))))
     "cartrige.restore")
    (setf *printer-storage* t-storage)))


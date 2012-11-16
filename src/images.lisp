;;;; images.lisp

(in-package #:eshop)

(defclass pic-cache ()
  (;; last update timestamp
   (last-update :initarg :last-update :initform (get-universal-time) :accessor last-update)
   ;; list of pics files
   (pics        :initarg :pics        :initform nil                  :accessor pics)))


(defvar *pics-cache* (make-hash-table :test #'equal)
  "Hash-table with caches of products' pics pathnames")

(defun pics.make-articul-subpath (articul)
  "Makes subpath like 129/129343 for storing pictures.
   Reason: ext3 filesystem restrictions for max number of files in directory"
  (ppcre:regex-replace  "(\\d{1,3})(\\d{0,})"  (format nil "~A" articul)  "\\1/\\1\\2"))

(defun pic-path (articul pic-name &optional (size "big"))
  "Compose path to pic (on hard drive) from pic's name and parent product's articul.
Path will be like path-to-pics/size/123/123456/picname.jpg
\(where 123456 - product's articul, and size is one of: big(default), goods, middle, minigoods, small)"
  (merge-pathnames (format nil "~A/~A/~A"
                           size
                           (pics.make-articul-subpath articul)
                           pic-name)
                   (config.get-option :paths :path-to-pics)))

(defun %get-pics-no-cache (key)
  "By given product key gets list of names of its pics in following format: pic-name.jpg;
Uses file operations, ignore cache"
  (let* ((path-art (pics.make-articul-subpath key))
         (path (format nil "~A/big/~A/*.jpg" (config.get-option :paths :path-to-pics) path-art)))
    (loop
       :for pic :in (ignore-errors (directory path))
       :collect (format nil "~A.~A"
                        (pathname-name pic)
                        (pathname-type pic)))))

(defun pics-cache-fresh-p (cache-timestamp)
  ;; TODO: make common cache freshness check (not only for pics)
  "Check whether timestamp is in range specified in config"
  (declare (number cache-timestamp))
  (< (get-universal-time) (+ cache-timestamp
                             (config.get-option :other-options :pics-cache-ttl))))

(defun drop-pics-cache (key)
  "Drops last-update time for pics cache for product to zero;
If cache instance doesn't exist, do nothing"
  (declare (string key))
  (awhen (gethash key *pics-cache*)
    (setf (last-update it) 0)))

(defun update-pics-cache (key)
  "Updates or create (if instance doesn't exist cache instance in hash-table;
Returns pics"
  (declare (string key))
  (when (getobj key 'product)
    (let ((instance (gethash key *pics-cache*)))
      (if instance
          (setf (last-update instance) (get-universal-time)
                (pics        instance) (%get-pics-no-cache key))
          ;; else create new instance
          (setf (gethash key *pics-cache*)
                (make-instance 'pic-cache
                               :last-update (get-universal-time)
                               :pics (%get-pics-no-cache key))))
      (pics (gethash key *pics-cache*)))))

(defmethod get-pics ((product product))
  (get-pics (key product)))

(defmethod get-pics ((key string))
  "By given product key gets list of names of its pics in following format: pic-name.jpg;
Firstly tries to get value from cache (checking for fresh)"
  (let ((cache-instance (gethash key *pics-cache*)))
    (if cache-instance
        (if (pics-cache-fresh-p (last-update cache-instance))
            ;; get value from cache
            (pics cache-instance)
            ;; else, update cache
            (update-pics-cache key))
        ;; else, create cache
        (update-pics-cache key))))


(defun get-dimensions (path-to-image)
  (let* ((string
          (let ((proc (sb-ext:run-program
                       "/usr/bin/identify"
                       (list "-format" "%w %h"
                             (format nil "~A" path-to-image)) :wait nil :output :stream)))
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

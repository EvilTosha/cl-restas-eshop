;;;; filters.lisp

(in-package #:eshop)

(class-core.make-class-and-methods
 filter
 ((:name key             :initform ""         :disabled t       :type string        :serialize t)
  ;; filtering function for applying to object/slot/etc, func's signature should be (foo object-list values-plist)
  (:name func            :initform nil        :disabled t       :type undefined     :serialize nil)
  (:name func-string     :initform ""         :disabled nil     :type string        :serialize t)
  ;; should be plist of strings (in fact it could be not only string, but any object that has
  ;; read/write methods)
  (:name data            :initform nil        :disabled nil     :type string-plist    :serialize t)
  ;; value, that will be used when no initial list/... supplied. Can be collection of objects,
  ;; another filter (will use its default-set) or type (will use storage of that type as collection))
  (:name default-set     :initform nil        :disabled nil     :type default-set   :serialize t)))


(defun filters.limit-start (items start)
  (nthcdr start items))

(defun filters.limit-end (items end)
  (if (> end (length items))
      items
      (subseq items 0 end)))

(defun filters.limit-region (items start length)
  (let ((end (+ start length)))
    (if (> end (length items))
        (filters.limit-start items start)
        (subseq items start end))))

(defun filters.limit-page (items page-size page-number)
  ;; numbering from 1
  (subseq items (* page-size (- page-number 1)) page-size))

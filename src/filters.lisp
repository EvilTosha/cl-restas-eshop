;;;; filters.lisp

(in-package #:eshop)

(defclass filter ()
  (;; type of objects for filtering (won't for on others), if any type accepted, objtype is t
   ;; type satisfying cheched via typep function
   (objtype     :initarg :objtype     :initform t        :accessor objtype     :type symbol)
   ;; filtering function for applying to object/slot/etc, func's signature should be (foo object value)
   (func        :initarg :func                           :accessor func        :type function)
   ;; if inclusive is nil, then only objects that not satisfy func are accepted
   (inclusive   :initarg :inclusive   :initform t        :accessor inclusive   :type bool)
   ;; additional info, that can be used in filter (like minimum price for product)
   (value       :initarg :value       :initform nil      :accessor value))
  (:documentation "Superclass for all filters, can be used itself for processing objects themselves"))

(defclass slot-filter (filter)
  (;; type of slot value (same conditions as for type of object in filter)
   (slot-type   :initarg :slot-type   :initform t        :accessor slot-type   :type symbol)
   ;; acccessor of the slot, which might be processed
   (slot        :initarg :slot                           :accessor slot        :type function))
  (:documentation "Filter for filtering objects by values of their specific slots"))


(defgeneric filters.check (object filter)
  (:documentation "Checks whether object satisfying filter, if it is, return object"))

(defmethod filters.check :around (object (filter filter))
  "Checks whether object is of appropriate type"
  (if (not (typep object (objtype filter)))
      (log5:log-for warning "Attempt to filter object of unappropriate type ~A (filter type: ~A)"
                    (type-of object) (type-of filter))
      ;; else
      (call-next-method)))

(defmethod filters.check (object (filter filter))
  (when (funcall (filters.include-transform (func filter) (inclusive filter))
                 object
                 (value filter))
    object))

(defmethod filters.check :around (object (filter slot-filter))
  "Checks whether object is of appropriate type"
  (if (not (typep (funcall (slot filter) object) (slot-type filter)))
      (log5:log-for warning "Attempt to filter object with slot of unappropriate type ~A (filter type: ~A)"
                    (type-of (funcall (slot filter) object)) (type-of filter))
      ;; else
      (call-next-method)))


(defmethod filters.check (object (filter slot-filter))
  (when (funcall (filters.include-transform (func filter) (inclusive filter))
                 (funcall (slot filter) object)
                 (value filter))
    object))

(defun filters.include-transform (func inclusive)
  "If inclusive return func, else return negotiation of it"
  (if inclusive func (complement func)))

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

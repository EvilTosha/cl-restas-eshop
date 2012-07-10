;;;; filters.lisp

(in-package #:eshop)

;; (class-core.make-class-and-methods
;;  filter
;;  ((:name key             :initform ""         :disabled t       :type string        :serialize t)
;;   ;; filtering function for applying to object/slot/etc, func's signature should be (foo object-list values-plist)
;;   (:name func            :initform nil        :disabled nil     :type function      :serialize nil)
;;   (:name func-text       :initform ""         :disabled nil     :type string        :serialize t)
;;   ;; should be plist of strings (in fact it could be not only string, but any object that has
;;   ;; read/write methods)
;;   (:name data            :initform nil        :disabled nil     :type data-plist    :serialize t)
;;   ;; value, that will be used when no initial list/... supplied. Can be collection of objects,
;;   ;; another filter (will use its default-set) or type (will use storage of that type as collection))
;;   (:name default-set     :initform nil        :disabled nil     :type default-set   :serialize t)))





;; (defclass filter ()
;;   ((id               :initarg :id                  :initform 0        :accessor )
;;    ;; type of objects for filtering (won't for on others), if any type accepted, objtype is t
;;    ;; type satisfying cheched via typep function
;;    (objtype          :initarg :objtype             :initform t        :accessor objtype     :type symbol)
;;    ;; filtering function for applying to object/slot/etc, func's signature should be (foo object value)
;;    (func             :initarg :func                           :accessor func        :type function)
;;    ;; additional info, that can be used in filter (like minimum price for product), if no value specified implicitly
;;    (default-value    :initarg :default-value       :initform nil      :accessor default-value))
;;   (:documentation "Superclass for all filters, can be used itself for processing objects themselves"))

;; (defclass slot-filter (filter)
;;   (;; type of slot value (same conditions as for type of object in filter)
;;    (slot-type   :initarg :slot-type   :initform t             :accessor slot-type   :type symbol)
;;    ;; acccessor of the slot, which might be processed
;;    (slot        :initarg :slot                                :accessor slot        :type function))
;;   (:documentation "Filter for filtering objects by values of their specific slots"))

;; (defclass option-filter (slot-filter)
;;   ((slot                              :initform #'optgroups   :accessor slot        :allocation :class)
;;    (optgroup    :initarg :optgroup    :initform ""            :accessor optgroup    :type string)
;;    (optname     :initarg :optname     :initform ""            :accessor optname     :type string)
;;    ;; type of filter, can be :checkbox, :radio, :range or :undefined (for now)
;;    (opt-type    :initarg :opt-type    :initform :undefined     :accessor opt-type))
;;   (:documentation "Filter for filtering products by specific option"))

;; (defmethod initialize-instance :after ((self option-filter)
;;                                        &rest initargs
;;                                        &key (opt-type nil opt-type-supplied)
;;                                        &allow-other-keys)
;;   (declare (ignore initargs))
;;   (when (and opt-type-supplied (not (equal :undefined opt-type)))
;;     (setf (func self) (filters.option-fn-designator opt-type))))

;; (defun filters.option-fn-designator (opt-type)
;;   (case opt-type
;;     (:checkbox #'filters.checkbox-fn)
;;     (:radio #'filters.radio-fn)
;;     (:range #'filters.range-fn)))

;; (defun filters.checkbox-fn (object value)
;;   "Function used for filtering checkbox-type options"
;;   ;; TODO: write function =)
;;   )

;; (defun filters.radio-fn (object value)
;;   "Function used for filtering radio-type options"
;;   ;; TODO: write function =)
;;   )

;; (defun filters.range-fn (object-value)
;;   "Function used for filtering range-type options"
;;   ;; TODO: write function =)
;;   )


;; (defgeneric filters.check (object filter &optional value)
;;   (:documentation "Checks whether object satisfying filter, if it is, return object"))

;; (defmethod filters.check :around (object (filter filter) &optional value)
;;   "Checks whether object is of appropriate type"
;;   (if (not (typep object (objtype filter)))
;;       (log5:log-for warning "Attempt to filter object of unappropriate type ~A (filter type: ~A)"
;;                     (type-of object) (type-of filter))
;;       ;; else
;;       (call-next-method)))

;; (defmethod filters.check (object (filter filter) &optional (value (default-value filter)))
;;   (when (funcall (func filter)
;;                  object
;;                  value)
;;     object))

;; (defmethod filters.check :around (object (filter slot-filter) &optional value)
;;   "Checks whether object is of appropriate type"
;;   (if (not (typep (funcall (slot filter) object) (slot-type filter)))
;;       (log5:log-for warning "Attempt to filter object with slot of unappropriate type ~A (filter type: ~A)"
;;                     (type-of (funcall (slot filter) object)) (type-of filter))
;;       ;; else
;;       (call-next-method)))


;; (defmethod filters.check (object (filter slot-filter) &optional (value (default-value filter)))
;;   (when (funcall (func filter)
;;                  (funcall (slot filter) object)
;;                  value)
;;     object))


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

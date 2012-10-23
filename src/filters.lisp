;;;; filters.lisp

(in-package #:eshop)

(class-core.make-class-and-methods
 filter
 ((:name key          :initform ""                                     :disabled t    :type string             :serialize t)
  (:name active       :initform t                                      :disabled nil  :type bool               :serialize t)
  (:name seo-text     :initform ""                                     :disabled nil  :type textedit           :serialize t)
  ;; hashtable of other filters and basic filters, for consequentally applying
  ;; Note: keys should be symbols
  (:name filters      :initform (make-hash-table :test #'equal)        :disabled nil  :type filters-hash-table :serialize t)
  ;; TODO: only groups allowed as parents for now, probably add arbitrary objects later
  (:name parents      :initform nil                                    :disabled nil  :type group-list         :serialize t)
  ;; should be plist of strings (in fact it could be not only string, but any object that has
  ;; read/write methods)
  (:name data         :initform nil                                    :disabled nil  :type string-plist       :serialize t)
  (:name serialize    :initform t                                      :disabled nil  :type bool               :serialize t)
  ;; value, that will be used when no initial list/... supplied. Can be collection (list for now) of objects,
  ;; another filter (will use its own default-set) or type (will use storage of that type as collection))
  (:name default-set  :initform 'product                               :disabled t  :type default-set        :serialize t)))

(defmethod name ((object filter))
  (getf (data object) :name))

;;; base class for deriving all basic-filter (such as option-range, or slot-exact-match, or custom-fn)
(class-core.make-class-and-methods
 basic-filter
 ((:name data         :initform nil                                        :disabled nil   :type string-plist :serialize t)
  (:name filter-type  :initform (error "filter-type should be specified")  :disabled t     :type symbol       :serialize t))
 :make-storage nil
 :instance-initforms (:filter-type 'undefined))

;;; (re)define needed methods
;;; TODO: move all to eshop-core package
(defmethod %unserialize :around ((type (eql 'basic-filter)) line)
  (let ((basic-filter (call-next-method)))
    ;;  return basic-filter with appropriate type
    (make-instance (filter-type basic-filter)
                   :data (data basic-filter)
                   :filter-type (filter-type basic-filter))))

(defclass basic-filter-data ()
  (;; id is equal to name of class of basic filter
   (type-id :initform (error "Id must be defined") :initarg :type-id :accessor type-id)
   ;; showing name of type (like Option Checkbox)
   (type-name :initform "" :initarg :type-name :accessor type-name)
   ;; hashtable of plists of specifiers of fields of basic-filter
   (fields :initform (make-hash-table :test #'equal) :initarg :fields :accessor fields)))

(defparameter *basic-filters-data* (make-hash-table :test #'equal)
  "Variable for storing instances of basic-filter-data, and convenient registering of basic filter types")

(defun filters.get-basics-types (&optional selected-type)
  (loop :for filter-data :being :the hash-values :in *basic-filters-data*
     :collect `(:name ,(type-name filter-data)
                      :value ,(format nil "~(~A~)" (type-id filter-data))
                      ,@(when (equal selected-type (type-id filter-data))
                              `(:selected "selected")))))

(defun filters.get-basic-fields (basic-id)
  (let ((data (gethash (anything-to-symbol basic-id) *basic-filters-data*)))
    (loop :for field :being :the hash-values :in (fields data)
       :using (hash-key key)
       :collect `(:name ,(getf field :name)
                        :value ,(format nil "~(~A~)" key)
                        ,@(when (equal 'multi (getf field :type))
                                `(:type  (format nil "~(~A~)" (getf field :type))))))))

(defun filters.register-basic-filter (type type-name &rest fields)
  (declare (symbol type))
  (let ((new-basic (make-instance 'basic-filter-data
                                  :type-id type :type-name type-name)))
    (sb-pcl::doplist (key val) fields
                     (setf (gethash (format nil "~A" key) (fields new-basic)) val))
    (setf (gethash type *basic-filters-data*) new-basic)))

(defgeneric filters.create-basic-filter (type &rest params)
  (:documentation "Method for convenient creating basic filters, without explicit make-instance.
Also show needed params in slime"))

(defgeneric filters.filter (filter &key obj-set outer-params)
  (:documentation "Filters given set of objects with given filter by given params;
Returns list of objects"))

(defmethod filters.filter ((filter basic-filter) &key obj-set outer-params)
  "Standard error method"
  (declare (ignore obj-set outer-params))
  (error "No filter function specified for current type of filter"))

;;; define all basic-filter classes
(defclass option-checkbox-filter (basic-filter) ()
  (:documentation "Filter for checking specified option for corresponding to given set of allowed values.
Needed keys (in params): :optgroup, :optname, :variant-[0-n] (:variant-0, :variant-1, ...)"))

(filters.register-basic-filter
 'option-checkbox-filter "Option Checkbox"
 :optgroup (list :name "Option group" :type 'single)
 :optname (list :name "Option name" :type 'single)
 :variants (list :name "Variants" :type 'multi :prefix "variant-"))

(defmethod filters.create-basic-filter ((type (eql 'option-checkbox-filter)) &rest params
                                        &key optgroup optname variants)
  (declare (string optgroup optname) (list variants))
  (let ((data
         (if variants
             ;; use specified list of variants
             (append (list :optgroup optgroup :optname optname)
                     (loop
                        :for variant :in variants
                        :for i :from 0
                        :collect (anything-to-keyword (format nil "variant-~D" i))
                        :collect variant))
             ;; else, use variants from params
             params)))
    (make-instance type :filter-type type :data data)))

(defmethod filters.filter ((filter option-checkbox-filter) &key obj-set outer-params)
  ;; TODO: use outer-params
  (declare (ignore outer-params) (list obj-set outer-params))
  (let* ((params (data filter))
         (optgroup (getf params :optgroup))
         (optname (getf params :optname))
         ;; FIXME: bad way to collect options
         (opt-variants (loop
                          :for i :from 0
                          :for variant-keyword := (anything-to-keyword
                                                   (format nil "variant~D" i))
                          :while (getf params variant-keyword)
                          :collect (getf params variant-keyword))))
    (remove-if-not
     #'(lambda (obj)
         (some #'(lambda (variant)
                   (equal variant (get-option obj optgroup optname)))
               opt-variants))
     obj-set)))


(defclass option-radio-filter (basic-filter) ()
  (:documentation "Filter for checking specified option for corresponding to one given value.
Needed keys (in params): :optgroup, :optname, :variants (like in checkbox)"))

(filters.register-basic-filter
 'option-radio-filter "Option Radio"
 :optgroup (list :name "Option group" :type 'single)
 :optname (list :name "Option name" :type 'single)
 :variants (list :name "Variants" :type 'multi :prefix "variant-"))

(defmethod filters.create-basic-filter ((type (eql 'option-radio-filter)) &rest params
                                        &key optgroup optname variants)
  (declare (string optgroup optname) (list variants))
  (let ((data
         (if variants
             ;; use specified list of variants
             (append (list :optgroup optgroup :optname optname)
                     (loop
                        :for variant :in variants
                        :for i :from 0
                        :collect (anything-to-keyword (format nil "variant-~D" i))
                        :collect variant))
             ;; else, use variants from params
             params)))
    (make-instance type :filter-type type :data data)))

(defmethod filters.filter ((filter option-radio-filter) &key obj-set outer-params)
  ;; TODO: use outer-params
  (declare (ignore outer-params) (list obj-set outer-params))
  (let* ((params (data filter))
         (optgroup (getf params :optgroup))
         (optname (getf params :optname))
         ;; FIXME: bad way to collect options
         (opt-variants (loop
                          :for i :from 0
                          :for variant-keyword := (anything-to-keyword
                                                   (format nil "variant~D" i))
                          :while (getf params variant-keyword)
                          :collect (getf params variant-keyword))))
    (remove-if-not
     #'(lambda (obj)
         (some #'(lambda (variant)
                   (equal variant (get-option obj optgroup optname)))
               opt-variants))
     obj-set)))

(defclass option-exact-match-filter (basic-filter) ()
  (:documentation "Filter for checking specified option for corresponding to given string value.
Needed keys (in params): :optgroup, :optname, :variant. Perform case-insensitive check."))

(filters.register-basic-filter
 'option-exact-match-filter "Option Exact Match"
 :optgroup (list :name "Option group" :type 'single)
 :optname (list :name "Option name" :type 'single)
 :variant (list :name "Value" :type 'single))

(defmethod filters.create-basic-filter ((type (eql 'option-exact-match-filter)) &rest params
                                        &key optgroup optname variant)
  (declare (string optgroup optname) (ignore optgroup optname variant))
  (make-instance type :filter-type type :data params))

;; both exact-match and substring types
;; are case insensitive for now
(defmethod filters.filter ((filter option-exact-match-filter) &key obj-set outer-params)
  ;; TODO: use outer-params
  (declare (ignore outer-params) (list obj-set outer-params))
  (let* ((params (data filter))
         (optgroup (getf params :optgroup))
         (optname (getf params :optname))
         (variant (getf params :variant)))
    (remove-if-not
     #'(lambda (obj)
         (string-equal (get-option obj optgroup optname) variant))
     obj-set)))

(defclass option-substring-filter (basic-filter) ()
  (:documentation "Filter for checking specified option for having as substring given string value.
Needed keys (in params): :optgroup, :optname, :variant. Perform case-insensitive check."))

(filters.register-basic-filter
 'option-substring-filter "Option Substring"
 :optgroup (list :name "Option group" :type 'single)
 :optname (list :name "Option name" :type 'single)
 :variant (list :name "Value" :type 'single))

(defmethod filters.create-basic-filter ((type (eql 'option-substring-filter)) &rest params
                                        &key optgroup optname variant)
  (declare (string optgroup optname) (ignore optgroup optname variant))
  (make-instance type :filter-type type :data params))

(defmethod filters.filter ((filter option-substring-filter) &key obj-set outer-params)
  ;; TODO: use outer-params
  (declare (ignore outer-params) (list obj-set outer-params))
  (let* ((params (data filter))
         (optgroup (getf params :optgroup))
         (optname (getf params :optname))
         (variant (getf params :variant)))
    (remove-if-not
     #'(lambda (obj)
         (search variant (get-option obj optgroup optname) :test #'char-equal))
     obj-set)))

(defclass option-have-filter (basic-filter) ()
  (:documentation "Filter for checking specified option for existance.
Needed keys (in params): :optgroup, :optname."))

(filters.register-basic-filter
 'option-have-filter "Option Exist"
 :optgroup (list :name "Option group" :type 'single)
 :optname (list :name "Option name" :type 'single))

(defmethod filters.create-basic-filter ((type (eql 'option-have-filter)) &rest params
                                        &key optgroup optname)
  (declare (string optgroup optname) (ignore optgroup optname))
  (make-instance type :filter-type type :data params))

(defmethod filters.filter ((filter option-have-filter) &key obj-set outer-params)
  ;; TODO: use outer-params
  (declare (ignore outer-params) (list obj-set outer-params))
  (let* ((params (data filter))
         (optgroup (getf params :optgroup))
         (optname (getf params :optname)))
    (remove-if-not
     #'(lambda (obj)
         (get-option obj optgroup optname))
     obj-set)))

(defclass option-range-filter (basic-filter) ()
  (:documentation "Filter for checking specified option value for being in specified range. Option must be number.
Needed keys (in params): :optgroup, :optname, :min (optional), :max (optional)."))

(filters.register-basic-filter
 'option-range-filter "Option Range"
 :optgroup (list :name "Option group" :type 'single)
 :optname (list :name "Option name" :type 'single)
 :min (list :name "Minimum value" :type 'single)
 :max (list :name "Maximum value" :type 'single))

(defmethod filters.create-basic-filter ((type (eql 'option-range-filter)) &rest params
                                        &key optgroup optname min max)
  (declare (string optgroup optname) ((or null number string) min max)
           (ignore params))
  (let ((data `(:optgroup ,optgroup :optname ,optname
                          ,@(when min (list :min (format nil "~D" min)))
                          ,@(when max (list :max (format nil "~D" max))))))
    (make-instance type :filter-type type :data data)))

(defmethod filters.filter ((filter option-range-filter) &key obj-set outer-params)
  ;; TODO: use outer-params
  (declare (ignore outer-params) (list obj-set outer-params))
  ;; TODO: range, interval, half-interval, equal
  (let* ((params (data filter))
         (optgroup (getf params :optgroup))
         (optname (getf params :optname))
         (min (awhen (getf params :min)
                (parse-float it)))
         (max (awhen (getf params :max)
                (parse-float it))))
    (declare ((or null number) min max))
    (remove-if-not
     #'(lambda (obj)
         (let* ((option-value (get-option obj optgroup optname))
                (value (if (and (valid-string-p option-value)
                                (stringp option-value))
                           (parse-float option-value)
                           option-value)))
           (when (numberp value)
             (and
              ;; perform min check
              (if min (< min value) t)
              ;; perform max check
              (if max (< value max) t)))))
     obj-set)))

(defclass slot-range-filter (basic-filter) ()
  (:documentation "Filter for checking specified slot-value for being in specified range. Option must be number.
Needed keys (in params): :slot-name, :slot-min (optional), :slot-max (optional)."))

(filters.register-basic-filter
 'slot-range-filter "Slot Range"
 :slot-name (list :name "Name of slot" :type 'single)
 :slot-min (list :name "Minimum value" :type 'single)
 :slot-max (list :name "Maximum value" :type 'single))

(defmethod filters.create-basic-filter ((type (eql 'slot-range-filter)) &rest params
                                        &key slot-name slot-min slot-max)
  (declare ((and (not null) (or string symbol)) slot-name) ((or null number string) slot-min slot-max)
           (ignore params))
  (let ((data `(:slot-name
                ,(format nil "~A" slot-name)
                ,@(when slot-min (list :slot-min (format nil "~D" slot-min)))
                ,@(when slot-max (list :slot-max (format nil "~D" slot-max))))))
    (make-instance type :filter-type type :data data)))

(defmethod filters.filter ((filter slot-range-filter) &key obj-set outer-params)
  ;; TODO: use outer-params
  (declare (ignore outer-params) (list obj-set outer-params))
  ;; TODO: range, interval, half-interval, equal
  (let* ((params (data filter))
         (slot-name (anything-to-symbol (getf params :slot-name)))
         (min (awhen (getf params :slot-min)
                (parse-float it)))
         (max (awhen (getf params :slot-max)
                (parse-float it))))
    (declare ((or null number) min max))
    (remove-if-not
     #'(lambda (obj)
         (let ((slot-value (slot-value obj slot-name)))
           (declare (number slot-value))
           (and
            ;; perform min check
            (if min (< min slot-value) t)
            ;; perform max check
            (if max (< slot-value max) t))))
     obj-set)))

(defclass slot-value-symbol-filter (basic-filter) ()
  (:documentation "Filter for checking specified slot-value (which must be symbol) for being equal to given value.
Needed keys (in params): :slot-name, :slot-value"))

(filters.register-basic-filter
 'slot-value-symbol-filter "Slot Symbol Equal"
 :slot-name (list :name "Name of slot" :type 'single)
 :slot-value (list :name "Symbol to check" :type 'single))

(defmethod filters.create-basic-filter ((type (eql 'slot-value-symbol-filter)) &rest params
                                        &key slot-name slot-value)
  (declare ((not null) slot-name slot-value)
           ((or string symbol) slot-name slot-value)
           (ignore params))
  (make-instance type :filter-type type :data (list :slot-name (format nil "~A" slot-name)
                                                    :slot-value (format nil "~A" slot-value))))

(defmethod filters.filter ((filter slot-value-symbol-filter) &key obj-set outer-params)
  ;; TODO: use outer-params
  (declare (ignore outer-params) (list obj-set outer-params))
  (let* ((params (data filter))
         (slot-name (anything-to-symbol (getf params :slot-name)))
         (slot-value (anything-to-symbol (getf params :slot-value))))
    (remove-if-not
     #'(lambda (obj)
         ;; format is for converting to string (as in params stored string value)
         (equal (slot-value obj slot-name) slot-value))
     obj-set)))

(defclass slot-value-string-filter (basic-filter) ()
  (:documentation "Filter for checking specified slot-value (which must be string) for being equal to given value.
Needed keys (in params): :slot-name, :slot-value"))

(filters.register-basic-filter
 'slot-value-string-filter "Slot String Equal"
 :slot-name (list :name "Name of slot" :type 'single)
 :slot-value (list :name "String to check" :type 'single))

(defmethod filters.create-basic-filter ((type (eql 'slot-value-string-filter)) &rest params
                                        &key slot-name slot-value)
  (declare ((not null) slot-name slot-value) ((or string symbol) slot-name)
           (symbol slot-value) (ignore params))
  (make-instance type :filter-type type :data (list :slot-name (format nil "~A" slot-name)
                                                    :slot-value (format nil "~A" slot-value))))
(defmethod filters.filter ((filter slot-value-string-filter) &key obj-set outer-params)
  ;; TODO: use outer-params
  (declare (ignore outer-params) (list obj-set outer-params))
  (let* ((params (data filter))
         (slot-name (anything-to-symbol (getf params :slot-name)))
         (slot-value (getf params :slot-value)))
    (remove-if-not
     #'(lambda (obj)
         ;; format is for converting to string (as in params stored string value)
         (string= (slot-value obj slot-name) slot-value))
     obj-set)))

(defclass function-filter (basic-filter)
  ((func :initarg :func :initform nil :accessor func))
  (:documentation "Filter for checking object for satisfying arbitrary function.
Function's signature should be #'(lambda (object &optional params) ...) and return t or nil.
Needed keys (in params): :func-text"))

(defmethod initialize-instance :after ((instance function-filter) &key)
  (setf (func instance) (eval (read-from-string (getf (data instance) :func-text)))))

(filters.register-basic-filter
 'function-filter "Function"
 :func-text (list :name "Text of function" :type 'single))

(defmethod filters.create-basic-filter ((type (eql 'function-filter)) &rest params
                                        &key func-text)
  (declare ((and (not null) string) func-text) (ignore params))
  (make-instance 'function-filter
                 :filter-type type
                 :data (list :func-text func-text)
                 :func (eval (read-from-string func-text))))

(defmethod filters.filter ((filter function-filter) &key obj-set outer-params)
  ;; TODO: use outer-params
  (declare (ignore outer-params) (list obj-set outer-params))
  (let ((params (data filter)))
    (remove-if-not
     #'(lambda (obj)
         (funcall (func filter) obj params))
     obj-set)))

(defclass filter-basic-filter (basic-filter)
  ((filter :initarg filter :initform nil :accessor filter))
  (:documentation "Proxy basic filter for common filter.
Needed keys (in params): :filter-key"))

(filters.register-basic-filter
 'filter-basic-filter "Normal filter"
 :filter-key (list :name "Key of filter" :type 'single))

(defmethod filters.create-basic-filter ((type (eql 'filter-basic-filter)) &rest params
                                        &key filter-key)
  (declare (ignore params))
  (aif (getobj filter-key 'filter)
       (make-instance 'filter-basic-filter
                      :filter-type type
                      :data (list :filter-key filter-key)
                      :func it)
       ;; else
       (error "Can't find filter with key ~A" filter-key)))

(defmethod filters.filter ((filter filter-basic-filter) &key obj-set outer-params)
  (filters.filter (getobj (getf (data filter) :filter-key) 'filter)
                  :obj-set obj-set :outer-params outer-params))

(defmethod filters.filter ((filter filter) &key (obj-set nil obj-set-supplied-p) outer-params)
  "Filters given set of objects with given filter by given params. Returns list of objects.
If no set specified, use default filter's set.
Params appended with default filter data before passing to filter function"
  (declare (default-set obj-set) (list outer-params))
  (let* ((set-identifier (if obj-set-supplied-p obj-set (default-set filter)))
         (set
          (etypecase set-identifier
            (list
             ;; list is appropriate set
             set-identifier)
            ;; symbol means storage
            ;; try to use such values as rare as possible, because each storage filtering
            ;; begins with processing and collecting storage to list
            (symbol
             (if (class-exist-p set-identifier)
                 (collect-storage set-identifier)
                 ;; else
                 (error "No such class ~A" set-identifier)))
            ;; filter means apply filter to result of running another filter
            ;; with its own default-set
            (filter
             (filters.filter set-identifier :outer-params outer-params)))))
    (when set
      (loop
         :for cur-set := set
         ;; FIXME: probably use filter's data slot somehow?
         :then (filters.filter filter-elt :obj-set cur-set :outer-params outer-params)
         :for filter-elt :being :the hash-values :in (filters filter)
         :while cur-set
         :finally (return cur-set)))))

(defun filters.check-object (filter object)
  "Checks single object for corresponding to given filter"
  (not (null (filters.filter filter :obj-set (ensure-list object)))))

(defun filters.count (filter &rest params &key obj-set outer-params)
  "Counts number of elements in filtered list"
  (declare (ignore obj-set outer-params))
  (length (apply #'filters.filter filter params)))

(defun filters.add-filter (filter inner-filter)
  (declare (filter filter) ((or filter basic-filter) inner-filter))
  ;; TODO: get rid of gensym
  (setf (gethash (gensym) (filters filter))
        inner-filter))

(defun filters.create-standard-filters ()
  "Creating standard filters, such as getters of children of object,
or filtrating by value of specific option of product"
  (mapcar #'(lambda (new-filter)
              (setobj (key new-filter) new-filter 'filter))
          (list
           ;; filter active products
           (make-instance 'filter
                          :key "active-products"
                          :default-set 'product)
           ;; get all child products of given group (key)
           (make-instance 'filter
                          :key "group-children"
                          :default-set 'product)))
  ;; add basic filters to newly created filters
  ;; FIXME: replace gensym with somesing else
  ;; probably write method for adding basic filters to filters
  (setf (gethash (gensym) (filters (getobj "active-products")))
        (filters.create-basic-filter 'slot-value-symbol-filter
                                     :slot-name 'active
                                     :slot-value t))
  (setf (gethash (gensym) (filters (getobj "group-children")))
        (filters.create-basic-filter 'function-filter
                                     :func-text
                                     "#'(lambda (obj &optional params)
                                         (let* ((group-key (getf params :group-key))
                                                (group (when (valid-string-p group-key)
                                                         (getobj group-key 'group))))
                                           (and group
                                                (equal group (parent obj)))))")))


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

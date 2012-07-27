;;;; filters.lisp

(in-package #:eshop)

(class-core.make-class-and-methods
 filter
 ((:name key          :initform ""                                     :disabled t    :type string           :serialize t)
  ;; filtering function for applying to object/slot/etc, func's signature should be (foo object-list values-plist);
  ;; also for objects satisfying filter, func should return object itself, not t or some other value
  (:name func         :initform nil                                    :disabled t    :type undefined        :serialize nil)
  ;; if func is basic function, func-data contains string of func; if func is composition of
  ;; other filters, func-data contains list of those filters
  (:name func-data    :initform ""                                     :disabled nil  :type filter-fn-data   :serialize t)
  ;; TODO: only groups allowed as parents for now, maybe add arbitrary objects later
  (:name parents      :initform nil                                    :disabled nil  :type group-list       :serialize t)
  ;; should be plist of strings (in fact it could be not only string, but any object that has
  ;; read/write methods)
  (:name data         :initform nil                                    :disabled nil  :type string-plist     :serialize t)
  ;; type of objects, to which filter can be applied to;
  ;; should be correct type that has own storage
  (:name objtype      :initform (error "Objtype should be specified")  :disabled nil  :type symbol           :serialize t)
  ;; value, that will be used when no initial list/... supplied. Can be collection (list for now) of objects,
  ;; another filter (will use its own default-set) or type (will use storage of that type as collection))
  (:name default-set  :initform nil                                    :disabled nil  :type default-set      :serialize t))
 :instance-initforms (:objtype 'undefined))

;; class for storing basic filters such as radio-option filter, checkbox-option filter, etc.
;; can't be used as standalone filter, the only usage is in slots of filters
;; Note: no need for checking objects' type, because basic filter can only be used within
;; filter's checks, and all typechecks are done there
(class-core.make-class-and-methods
 basic-filter
 (;; type of filter ('option-range, 'option-checkbox, etc.)
  (:name filter-type  :initform (error "Filter-type should be specified") :disabled nil  :type symbol           :serialize t)
  ;; same meaning as slot data in filter class
  (:name data         :initform nil                                       :disabled nil  :type string-plist     :serialize t))
 :instance-initforms (:filter-type 'undefined)
 :make-storage nil)

(defgeneric filters.filter (filter &key obj-set outer-params)
  (:documentation "Filters given set of objects with given filter by given params;
Returns list of objects"))

(defmethod filters.filter ((filter basic-filter) &key obj-set outer-params)
  ;; Note: obj-set must be specified for basic-filter
  (declare ((and (not null) default-set) obj-set) (list outer-params)
           (ignore outer-params))
  ;; FIXME: now basic filters doesn't use outer-params, because
  ;; it only do work for marketing filters, not fullfilters
  (let* ((params (data filter))
         (optgroup (awhen (getf params :optgroup) it))
         (optname (awhen (getf params :optname) it))
         (opt-variants (loop
                          :for i :from 0
                          :for variant-keyword := (anything-to-keyword
                                                   (format nil "variant~D" i))
                          :while (getf params variant-keyword)
                          :collect (getf params variant-keyword))))
    ;; basic-filter doesn't have 'func slot so it could be applied to each object in list
    (remove-if-not
     ;; should return #'(lambda (obj) ...)
     (ecase (filter-type filter)
       (option-checkbox
        ;; FIXME: bad way to collect options
        #'(lambda (obj)
            (let ((option (get-option obj optgroup optname)))
              (some #'(lambda (variant)
                        (equal variant option))
                    opt-variants))))
       (option-radio
        #'(lambda (obj)
            (equal (get-option obj optgroup optname) (getf params :variant))))
       (option-exact-match
        #'(lambda (obj)
            (equal (get-option obj optgroup optname) (getf params :variant))))
       (option-substring
        #'(lambda (obj)
            (search (getf params :variant) (get-option obj optgroup optname))))
       (option-has
        #'(lambda (obj)
            (get-option obj optgroup optname)))
       (price-range
        #'(lambda (obj)
            (< (parse-integer (getf params :price-min))
               (siteprice obj)
               (parse-integer (getf params :price-max))))))
     obj-set)))


(defmethod filters.filter ((filter filter) &key obj-set outer-params)
  "Filters given set of objects with given filter by given params. Returns list of objects.
If no set specified, use default filter's set.
Params appended with default filter data before passing to filter function"
  (declare (default-set obj-set) (list outer-params))
  (let* ((set-identifier (if obj-set obj-set (default-set filter)))
         (params (append outer-params (data filter)))
         (set
          (etypecase set-identifier
            (list
             ;; check types of objects
             (if (notevery #'(lambda (obj)
                               (typep obj (objtype filter)))
                           set-identifier)
                 (error "Unappropriate type of objects in list")
                 ;; else
                 ;; list is appropriate set
                 set-identifier))
            ;; symbol means storage
            ;; try to use such values as rare as possible, because each storage filtering
            ;; begins with processing and collecting storage to list
            (symbol
             (if (not (equal set-identifier (objtype filter)))
                 (error "Unappropriate type of objets ~A" set-identifier)
                 ;; else
                 (if (class-exist-p set-identifier)
                     (collect-storage set-identifier)
                     ;; else
                     (error "No such class ~A" set-identifier))))
            ;; filter means apply filter to result of running another filter
            ;; with its own default-set
            (filter
             (if (not (equal (objtype filter) (objtype set-identifier)))
                 (error "Conflictng objtypes of filters ~A and ~A"
                        (objtype filter) (objtype set-identifier))
                 ;; else
                 (filters.filter set-identifier :outer-params outer-params))))))
    (if (null set)
        nil
        ;; else
        (etypecase (func-data filter)
          ;; if 'func-data slot is string, just use 'func slot
          (string (funcall (func filter) set params))
          ;; if 'func-data slot is list of filters (and basic-filters) consequentally apply them to set
          (list (loop
                   :for cur-set := set
                   :then (filters.filter filter-elt :obj-set cur-set :outer-params outer-params)
                   :for filter-elt :in (func-data filter)
                   :while cur-set
                   :finally (return cur-set)))))))

(defun filters.count (filter &key obj-set outer-params)
  "Counts number of elements in filtered list"
  (length (filters.filter filter :obj-set obj-set :outer-params outer-params)))

(defun filters.check-object (filter object &optional outer-params)
  "Checks whether object satisfies given filter.
This functions shouldn't be used frequently, use filters.filter function for lists instead."
  (declare (filter filter) (list outer-params))
  ;; check object type
  (if (not (typep object (objtype filter)))
      (error "Object isn't of type ~A" (objtype filter))
      ;; else
      (funcall (func filter) (list object) outer-params)))

(defun filters.create-standard-filters ()
  "Creating standard filters, such as getters of children of object,
or filtrating by value of specific option of product"
  (mapcar #'(lambda (new-filter)
              (setobj (key new-filter) new-filter 'filter))
          (list
           ;; filter active products
           (make-instance 'filter
                          :key "active-products"
                          :func #'(lambda (product-list params)
                                    (declare (list product-list params) (ignore params))
                                    (remove-if-not #'active product-list))
                          :objtype 'product
                          :default-set 'product)
           ;; get all child products of given group (key)
           (make-instance 'filter
                          :key "group-children"
                          :func #'(lambda (object-list params)
                                    (declare (list object-list params))
                                    (let ((parent-key (getf params :parent-key)))
                                      (remove-if-not
                                       #'(lambda (obj)
                                           (some #'(lambda (parent)
                                                     (string= (key parent) parent-key))
                                                 (parents obj)))
                                       object-list)))
                          :objtype 'product
                          :default-set 'product))))



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

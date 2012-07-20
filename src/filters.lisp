;;;; filters.lisp

(in-package #:eshop)

(class-core.make-class-and-methods
 filter
 ((:name key          :initform ""                                     :disabled t    :type string        :serialize t)
  ;; filtering function for applying to object/slot/etc, func's signature should be (foo object-list values-plist);
  ;; also for objects satisfying filter, func should return object itself, not t or some other value
  (:name func         :initform nil                                    :disabled t    :type undefined     :serialize nil)
  (:name func-string  :initform ""                                     :disabled nil  :type string        :serialize t)
  ;; should be plist of strings (in fact it could be not only string, but any object that has
  ;; read/write methods)
  (:name data         :initform nil                                    :disabled nil  :type string-plist  :serialize t)
  ;; type of objects, to which filter can be applied to;
  ;; should be correct type that has own storage
  (:name objtype      :initform (error "Objtype should be specified")  :disabled nil  :type symbol        :serialize t)
  ;; value, that will be used when no initial list/... supplied. Can be collection (list for now) of objects,
  ;; another filter (will use its own default-set) or type (will use storage of that type as collection))
  (:name default-set  :initform nil                                    :disabled nil  :type default-set   :serialize t))
 :instance-initforms (:objtype 'undefined)
 ;; just for testing, remove later
 :serialize nil)

(defun filters.filter (filter &key obj-set outer-params)
  "Filters given set of objects with given filter by given params. Returns list of parameters.
If no set specified, use default filter's set.
Params appended with default filter data before passing to filter function"
  (declare (filter filter) ((or nil default-set) obj-set) (list outer-params))
  (let ((set (if obj-set obj-set (default-set filter)))
        (params (append outer-params (data filter))))
    (etypecase set
      (list
       ;; check types of objects
       (if (notevery #'(lambda (obj) (typep obj (objtype filter))) set)
           (error "Unappropriate type of objects in list")
           ;; else
           (funcall (func filter) set params)))
      ;; symbol means storage
      ;; try to use such values as rare as possible, because each storage filtering
      ;; begins with processing and collecting storage to list
      (symbol
       (if (not (equal set (objtype filter)))
           (error "Unappropriate type of objets ~A" set)
           ;; else
           (if (class-exist-p set)
               (funcall (func filter) (collect-storage set) params)
               ;; else
               (error "No such class ~A" set))))
      ;; filter means apply filter to result of running another filter
      ;; with its own default-set
      (filter
       (if (not (equal (objtype filter) (objtype set)))
           (error "Conflictng objtypes of filters ~A and ~A"
                  (objtype filter) (objtype set))
           ;; else
           (funcall (func filter)
                    (filters.filter set :outer-params outer-params)
                    params))))))

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
  )

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

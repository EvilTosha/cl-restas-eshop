;;;; list-filters.lisp

(in-package #:eshop)

(defclass field-filter ()
  ((fieldacc           :initarg :fieldacc         :initform nil       :accessor fieldacc)
   ;; func for type of filtering field. Unused for now (always #'list-filter.field-filter)
   (filtertype         :initarg :filtertype       :initform nil       :accessor filtertype)
   ;; func for apply to field value. For example #'> or #'string/= (lambda if necessary)
   (filterfunc         :initarg :filterfunc       :initform nil       :accessor filterfunc)
   (inclusive          :initarg :inclusive        :initform t         :accessor inclusive)
   (value              :initarg :value            :initform nil       :accessor value)))


(defmacro list-filters.inclusive-check (inclusive expression value)
  "Returns value if inclusive and expression are both t or both nil"
  `(let ((incl ,inclusive)
         (expr ,expression)
         (val ,value))
     (when (or (and incl expr)
               (not (or incl expr)))
       val)))

(defun list-filters.filter-list (items &rest filters)
  "Filters given list with given filters"
  (remove-if
   #'null
   (mapcar
    #'(lambda (item)
        (let ((res item))
          (loop for filter in filters
             do
               (setf res
                     (and res
                          (apply (filtertype filter) (list item filter)))))
          res))
    items)))


(defun list-filters.field-filter (item filter)
  (list-filters.inclusive-check
   (inclusive filter)
   (apply (filterfunc filter) (list (funcall (fieldacc filter) item)
                                    (value filter)))
   item))

(defun list-filters.limit-start (items start)
  (nthcdr start items))

(defun list-filters.limit-end (items end)
  (if (> end (length items))
      items
      (subseq items 0 end)))

(defun list-filters.limit-region (items start length)
  (let ((end (+ start length)))
    (if (> end (length items))
        (list-filters.limit-start items start)
        (subseq items start end))))

(defun list-filters.limit-page (items page-size page-number)
  ;; numbering from 1
  (subseq items (* page-size (- page-number 1)) page-size))

(defun list-filters.sort-list (list json-sorters)
  (if json-sorters
      (let ((sorters (mapcar #'servo.alist-to-plist (decode-json-from-string json-sorters))))
        list)
      ;; else
      list))

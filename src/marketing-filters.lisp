;;;; marketing-filters.lisp

(in-package #:eshop)

(defun marketing-filters.group-children (group &optional (showall nil))
  (declare (group group) (boolean showall))
  (if showall
      (products group)
      (filters.filter (getobj "active-products" 'filter) :obj-set (products group))))

(defun marketing-filters.render-filters (group &optional (showall nil))
  "Rendering of marketing filters"
  (declare (group group))
  (let* ((products (marketing-filters.group-children group showall))
         (filters (marketing-filters.get-filters group products)))
    (when filters
      ;; FIXME: write proper render method (common for all filters)
      (list (soy.fullfilter:rightfilter
             (list :filters (mapcar #'(lambda (pair)
                                        (list :name (getf (data (car pair)) :name)
                                              :key (key (car pair))
                                              :parentkey (key group)
                                              :num (cdr pair)))
                                    filters)))))))

(defgeneric marketing-filters.get-filters (object products)
  (:documentation "Returns list of non-null filters and number of products in result"))

(defmethod marketing-filters.get-filters ((group group) (products list))
  (marketing-filters.get-filters (filters group) products))

(defmethod marketing-filters.get-filters ((filters list) (products list))
  (remove-if #'null
             (mapcar #'(lambda (filter)
                         (let ((num (filters.count filter :obj-set products)))
                           (when (plusp num)
                             (cons filter num))))
                     filters)))

;;; marketing filters

(defun marketing-filters.bind-filter (group default-filter)
  "Make copy of default-filter with unique key, and with group as parent,
then bind group and new filter to each other;
Filter's key is concatenated group's and default-filter's keys"
  (declare (group group) (filter default-filter))
  (let* ((key (format nil "~A-~A" (key group) (key default-filter)))
         (filter default-filter))
    ;; setup filter's slots
    (setf (key filter) key
          (parents filter) (list group))
    ;; add filter to group's filters slot
    (setf (gethash key (filters group)) filter)
    (setobj key filter 'filter)))

;; TODO: make recreating/restoring methos(s)
(defun marketing-filters.create-all-filters ()
  "Creating all marketing filters in system"


(defun mareketing-filters.default-sale-filter ()
  "Creating \"sale\" default filter"
  (make-instance 'filter
                 :key "sale"
                 :func #'groupd.is-groupd
                 :objtype 'product
                 :default-set 'products
                 :data (list :name "Ликвидация склада!")))

(defun marketing-filters.default-bestprice-filter ()
  (make-instance 'filter
                 :key "bestprice"
                 :func #'(lambda (object) (plusp (delta-price object)))
                 :objtype 'product
                 :default-set 'products
                 :data (list :name "Горячий уик-энд скидок")))

(defun marketing-filters.create-ipad3-filter ()
  (let* ((group (getobj "planshetnie-komputery" 'group))
         (filter
          (make-instance 'filter
                         :key "ipad-filter"
                         ;; list of one basic-filter
                         :func-data (list (make-instance
                                           'basic-filter
                                           :filter-type 'exact-match
                                           :data (list :optgroup "Общие характеристики"
                                                       :optname "Модель"
                                                       :variant "ipad new")))
                         :objtype 'product
                         :default-set (marketing-filters.group-children group)
                         :parents (list group)
                         :data (list :name "IPad 3"))))
    (marketing-filters.bind-filter group filter)))

(defun marketing-filters.create-ultrabooks-filter ()
  (let* ((group (getobj "noutbuki" 'group))
         (filter
          (make-instance 'fitler
                         :key "ultrabooks-filter"
                         :func-data (list (make-instance
                                           'basic-filter
                                           :filter-type 'exact-match
                                           :data (list :optgroup "Общие характеристики"
                                                       :optname "Тип устройства"
                                                       :variant "Ультрабук")))
                         :objtype 'product
                         :default-set (marketing-filters.group-children group)
                         :parents (list group)
                         :data (list :name "Ультрабуки"))))
    (marketing-filters.bind-filter group filter)))

(defun report.set-filters (groups filter-func name filter-key)
  (mapcar #'(lambda (gr)
              (edit-marketing-filter gr filter-key name filter-func))
          groups))

(defun report.create-marketing-filters ()
	(marketing-filters.create-ipad3-filter)
  (marketing-filters.create-ultrabooks-filter)
  ;; TODO: убрать костыль
	(report.set-filters (collect-storage 'group)
											#'groupd.holiday.is-groupd
											"Для отдыха"
											"holidays"))

(defun report.set-salefilter ()
  (mapcar #'(lambda (v)
              (create-sale-filter (getobj (format nil "~a" v) 'group)))
          (list "netbuki"
                "noutbuki"
                "planshetnie-komputery"
                "cifrovye-fotoapparaty"
                "lcd-televizory"
                "monitory"
                "printery"
                "mfu"
                "myshki"
                "klaviatury"
                "holodilniki-i-morozilniki"
                "stiralnie-mashiny")))

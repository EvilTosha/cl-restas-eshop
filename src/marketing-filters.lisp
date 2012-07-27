;;;; marketing-filters.lisp

(in-package #:eshop)

(defun marketing-filters.group-children (group &optional (showall nil))
  (declare (group group) (boolean showall))
  (if showall
      (products group)
      (filters.filter (getobj "active-products" 'filter) :obj-set (products group))))

(defun marketing-filters.render-filters (group &optional (showall nil))
  "Rendering of one-click filters"
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

(defun filters.proccess-xls (xls-file line-proccess-func &key (ignor-head-line t))
  (log5:log-for info "proccess:: ~a" xls-file)
  (let ((num 0))
    (xls.restore-from-xls
     (merge-pathnames xls-file)
     #'(lambda (line)
         (let* ((args (sklonenie-get-words line)))
           (incf num)
           (let ((vendor (gethash (string-downcase (cadr words)) *vendor-storage*)))
             (when vendor
               (log5:log-for info "~a|~a|~a|~a|~a"
                             num (car words) (cadr words) vendor (length words))
               (setf (name vendor) (cadr words)))
             ;; (unless vendor
             ;;   (setf vendor (make-instance 'vendor :key (string-downcase (cadr words))
             ;;                               :name (string-downcase (cadr words)))))
             ;; (setf (gethash (car words) (seo-texts vendor)) (caddr words))
             ;; (setf (gethash (string-downcase (cadr words)) *vendor-storage*) vendor)
             ))))))


;;; marketing filters

(defun edit-marketing-filter (group key-suffix name func)
  (let* ((key (format nil "~A-~A" (key group) key-suffix))
         new-filter
         (filter (aif (find key (filters group) :test #'equal :key #'key)
                      it
                      (progn
                        (setf new-filter t)
                        (setobj key (make-instance 'filter))))))
    (setf (name filter) name)
    (setf (func filter) func)
    (setf (key filter) key)
    (setf (parents filter) (list group))
    (when new-filter
      (push filter (filters group)))
    filter))

(defun create-sale-filter (group)
  (edit-marketing-filter
   group "sale" "Ликвидация склада!" #'groupd.is-groupd))

(defun create-bestprice-filter (group)
  (edit-marketing-filter
   group "bestprice" "Горячий уик-энд скидок"
   #'(lambda (object) (plusp (delta-price object)))))

(defun create-ipad3-filter (group)
  (edit-marketing-filter
   group "ipad3" "IPad 3"
   #'(lambda (p)
       (string= "ipad new"
                (format nil "~(~A~)"
                        (get-option p "Общие характеристики" "Модель"))))))

(defun create-man-sale-filter (group)
  (edit-marketing-filter
   group "23feb" "Подарки к 23 февраля" #'groupd.man.is-groupd))

(defun create-woman-sale-filter (group)
  (edit-marketing-filter
   group "8mart" "Подарки к 8 марта" #'groupd.woman.is-groupd))

(defun report.set-man-salefilter ()
  (process-storage #'create-man-sale-filter 'group))

(defun report.set-woman-salefilter ()
  (process-storage #'create-woman-sale-filter 'group))


(defun report.set-filters (groups filter-func name filter-key)
  (mapcar #'(lambda (gr)
              (edit-marketing-filter gr filter-key name filter-func))
          groups))

(defun report.create-marketing-filters ()
	(create-ipad3-filter (getobj "planshetnie-komputery" 'group))
	(report.set-filters (list (getobj "noutbuki" 'group))
											#'(lambda (product)
													(equal (get-option product
                                             "Общие характеристики"
                                             "Тип устройства")
                                 "Ультрабук"))
											"Ультрабуки"
											"ultrabooks")
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

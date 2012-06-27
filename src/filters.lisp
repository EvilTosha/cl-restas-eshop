;;;; filters.lisp

(in-package #:eshop)

;;Возвращает длину списка активных продуктов-потомков подходящих под фильтр
(defun get-filtered-product-list-len (object filter)
  (length (remove-if-not (func filter)
                         (remove-if-not #'active
                                        (storage.get-recursive-products object)))))

(defun is-empty-filtered-list (object filter)
  (zerop (get-filtered-product-list-len object filter)))


;;Составление строки для представления фильтров в 1 клик на странице с fullfilter
(defun filters.make-string-filter (filter num &optional itsme)
  (if itsme
      (format nil "<b>~a</b> (~a)<br/>"
              (name filter)
              num)
      (format nil "<a class=\"rightfilter\" href=\"/~a/~a\">~a</a> (~a)<br/>"
              (key (parent filter))
              (key filter)
              (name filter)
              num)))

;;количество непустых фильтров у группы
(defun num-nonempty-filters (object)
  (length (remove-if #'(lambda (fil) (is-empty-filtered-list object fil))
                     (filters object))))

(defmethod filters.get-filters ((group group) (products list))
  "Возвращает список ненулевых фильтров на списке объектов и количество объесктов в выборке"
  (filters.get-filters (filters group) products))

(defmethod filters.get-filters ((filters list) (products list))
  "Возвращает список ненулевых фильтров на списке объектов и количество объесктов в выборке"
  (remove-if #'null (mapcar #'(lambda (filter)
                                (let ((num (length
                                            (remove-if-not (func filter) products))))
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


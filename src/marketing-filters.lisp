;;;; marketing-filters.lisp

(in-package #:eshop)

(defun marketing-filters.group-children (group &optional (showall nil))
  (declare (group group))
  (if showall
      (products group)
      (filters.filter (getobj "active-products" 'filter) :obj-set (products group))))

(defun marketing-filters.%prepare-filters (group filters &optional (showall nil))
  "Rendering of marketing filter"
  (declare (group group))
  ;; FIXME: write proper render method (common for all filters)
  (mapcar #'(lambda (pair)
              (list :name (getf (data (car pair)) :name)
                    :key (key (car pair))
                    :parentkey (key group)
                    :num (cdr pair)
                    :showall showall))
          filters))

(defun marketing-filters.render-filters (group &optional (showall nil))
  "Rendering of marketing filters"
  (declare (group group))
  (let* ((products (marketing-filters.group-children group showall))
         (filters (marketing-filters.get-filters group products)))
    (when filters
      (list (soy.fullfilter:rightfilter
             (list :filters (marketing-filters.%prepare-filters group filters showall)))))))

(defun marketing-filters.get-vendor-filters (group vendor products)
    (declare (group group) (vendor vendor))
    (loop
       :for filter :being :the hash-values :in (filters group)
       :for num := 0
       :when (progn
               (when (search (key vendor) (key filter))
                 (setf num (filters.count filter :obj-set products)))
               (plusp num))
       :collect (cons filter num)))

(defun marketing-filters.render-vendor-filters (group vendor &optional (showall nil))
  "Rendering of marketing filters"
  (declare (group group) (vendor vendor))
  (let* ((products (remove-if-not (rcurry #'string= (name vendor))(marketing-filters.group-children group showall) :key #'vendor))
         (filters (marketing-filters.get-vendor-filters group vendor products)))
    ;; (debug-slime-format "group:~A vendor:~A products:~A filters:~A" (key group) vendor (length products) (length filters))))
    (when filters
      (soy.fullfilter:rightfilterserias
       (list :filters (marketing-filters.%prepare-filters group filters showall))))))

(defgeneric marketing-filters.get-filters (object products)
  (:documentation "Returns list of non-null filters and number of products in result"))

(defmethod marketing-filters.get-filters ((group group) (products list))
  (marketing-filters.get-filters (filters group) products))

(defmethod marketing-filters.get-filters ((filters hash-table) (products list))
  (loop
     :for filter :being :the hash-values :in filters
     :for num := 0
     :when (progn
             (when (active filter)
               (setf num (filters.count filter :obj-set products)))
             (plusp num))
     :collect (cons filter num)))

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


(defun marketing-filters.%create-util-filter (group discount)
  "Creating \"util\" default filter"
  (let* ((key (format nil "~A-util-~A-filter" (key group) discount))
         (filter (make-instance 'filter
                                :key key
                                :active t
                                :parents (list group)
                                :default-set 'product
                                :data (list :name (format nil "Скидка ~Aр по акции!" discount))
                                :serialize nil)))
    ;; setup basic filters
    ;; TODO: get rid of gensym
    (setf (gethash (gensym) (filters filter))
          (filters.create-basic-filter
           'function-filter
           :func-text
           (format nil "#'(lambda (obj &optional params)
                             (declare (ignore params))
                             (search \"~A\"
                                  (get-option obj \"Secret\" \"Продающий текст\")))" discount))
          ;; add filter to group's filters slot
          (gethash key (filters group)) filter)
    (setobj key filter 'filter)))

(defun marketing-filters.create-sale-filter (group)
  "Creating \"sale\" default filter"
  (let* ((key (format nil "~A-sale-filter" (key group)))
         (filter (make-instance 'filter
                                :key key
                                :active nil
                                :parents (list group)
                                :default-set 'product
                                :data (list :name "Акционные товары!")
                                :serialize nil)))
    ;; setup basic filters
    ;; TODO: get rid of gensym
    (setf (gethash (gensym) (filters filter))
          (filters.create-basic-filter 'function-filter
                                       :func-text
                                       "#'(lambda (obj &optional params)
                                           (declare (ignore params))
                                           (groupd.is-groupd obj))")
          ;; add filter to group's filters slot
          (gethash key (filters group)) filter)
    (setobj key filter 'filter)))

(defun marketing-filters.create-bestprice-filter (group)
  (let* ((key (format nil "~A-bestprice-filter" (key group)))
         (filter (make-instance 'filter
                                :key key
                                :active nil
                                :parents (list group)
                                :default-set 'product
                                :data (list :name "Лучшие цены!")
                                :serialize nil)))
    ;; setup basic filters
    ;; TODO: get rid of gensym
    (setf (gethash (gensym) (filters filter))
          (filters.create-basic-filter 'slot-range-filter
                                       :slot-name 'delta-price
                                       :slot-min 0)
          ;; add filter to group's filters slot
          (gethash key (filters group)) filter)
    (setobj key filter 'filter)))

(defun marketing-filters.set-holiday-filter (group)
  (let* ((key (format nil "~A-holiday-filter" (key group)))
         (filter (make-instance 'filter
                                :key key
                                :parents (list group)
                                :default-set 'product
                                :data (list :name "Для отдыха!")
                                :serialize nil)))
    ;; setup basic filters
    ;; TODO: get rid of gensym
    (setf (gethash (gensym) (filters filter))
          (filters.create-basic-filter 'function-filter
                                       :func-text
                                       "#'(lambda (obj &optional params)
                                           (declare (ignore params))
                                           (groupd.holiday.is-groupd obj))")
          ;; add filter to group's filters slot
          (gethash key (filters group)) filter)
    (setobj key filter 'filter)))

(defun %get-serias (group)
  "Prepare list of pars seria name & vendor name"
  (declare (group group))
  (remove-duplicates
   (loop
      :for product :in (products group)
      :for seria := (get-option product "Общие характеристики" "Линейка")
      :when seria
      :collect (let ((vendor (getobj (string-downcase (vendor product)) 'vendor)))
                 (cons seria vendor))) :test #'equal))

(defun marketing-filters.%create-serias-marketing-filter (group para)
  "Creating \"serias\" default filter"
  (declare (group group))
  (let ((seria (car para))
        (vendor (cdr para))
        (filter))
    (when (and (valid-string-p seria)
               vendor)
      (let ((key (concatenate 'string
                              (key group)
                              "-"
                              (key vendor)
                              "-seria-"
                              (hunchentoot:url-encode
                               (ppcre:regex-replace-all " "
                                                        (string-downcase seria) "-"))))
            (name (concatenate 'string  (key vendor) " " seria)))
        (debug-slime-format "~&~A: ~A" key name)
        (setf filter (make-instance 'filter
                                    :key key
                                    :active nil
                                    :parents (list group)
                                    :default-set 'product
                                    :data (list :name name)
                                    :serialize t))
        ;; setup basic filters
        ;; TODO: get rid of gensym
        (setf (gethash (gensym) (filters filter))
              (filters.create-basic-filter
               'function-filter
               :func-text
               (format nil "#'(lambda (obj &optional params)
                             (declare (ignore params))
                             (string= (get-option obj ~S ~S) ~S))"
                       "Общие характеристики" "Линейка" seria))
              ;; add filter to group's filters slot
              (gethash key (filters group)) filter)
        (setobj key filter 'filter)))))


(defun marketing-filters.%create-serias-marketing-filters (group)
  "Creating \"seria\" default filters"
  (declare (group group))
  (loop
     :for para :in (%get-serias group)
     :do (marketing-filters.%create-serias-marketing-filter group para)))

;; TODO: make recreating/restoring methos(s)
(defun marketing-filters.create-all-filters ()
  "Creating all marketing filters in system"
  (process-storage
   #'(lambda (group)
       ;; (marketing-filters.%create-serias-marketing-filters group)
       (marketing-filters.create-bestprice-filter group)
       (marketing-filters.set-holiday-filter group))
   'group)
  ;; TODO: убрать костыль
  (marketing-filters.%create-util-filter (getobj "komputery" 'group) 1000)
  (marketing-filters.%create-util-filter (getobj "komputery" 'group) 2000)
  (marketing-filters.%create-util-filter (getobj "komputery" 'group) 5000)
  (mapcar #'(lambda (group-key)
              (marketing-filters.create-sale-filter (getobj group-key 'group)))
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
                "stiralnie-mashiny"
                "konvektory"
                "radiatori"
                "mobilephones")))

(defun marketing-filters.get-seria-filters ()
  (collect-storage 'filter :when-fn #'(lambda (filter) (getf (data filter) :seria))))

;;;; render.lisp

(in-package #:eshop)

(defclass eshop-render () ())

(setf *default-render-method* (make-instance 'eshop-render))

(defun render.breadcrumbs (in &optional out)
  "Processing parents until nil, creating breadcrumbs"
  (if in
      (progn
        (if (productp in)
            (push (list :key (articul in) :val (name-seo in)) out)
            (push (list :key (key in) :val (name in)) out))
        (render.breadcrumbs (parent in) out))
      ;; else -  end of recursion
      (list :breadcrumbelts (butlast out)
            :breadcrumbtail (car (last out)))))

(defun menu-sort (a b)
  "Function for sorting groups by order field"
  (when (and (order a) (order b))
    (< (order a)
       (order b))))

;;TODO временно убрана проверка на пустые группы, тк это поле невалидно
(defun render.menu (&optional current-object)
  "Creating left menu"
  (let* ((current-root (get-root-parent current-object))
         (dividers (list "setevoe-oborudovanie" "foto-and-video" "rashodnye-materialy"))
         (src-lst
          (mapcar #'(lambda (val)
                      (if (and current-root
                               (equal (key val) (key current-root)))
                          ;; This is current
                          (soy.menu:selected
                           (list :divider (member (key val) dividers :test #'equal)
                                 :key (key val)
                                 :name (name val)
                                 :icon (icon val)
                                 :subs (loop
                                          :for child
                                          :in (sort
                                               (remove-if #'(lambda (g)
                                                              (or
                                                               ;; (empty g)
                                                               (not (active g))))
                                                          (groups val))
                                               #'menu-sort)
                                          :collect
                                          (list :key (key child) :name (name child)))))
                          ;; else - this is ordinal
                          (soy.menu:ordinal (list :divider (member (key val) dividers :test #'equal)
                                                  :key  (key val)
                                                  :name (name val)
                                                  :icon (icon val)))))
                  (get-root-groups))))
    (soy.menu:main (list :elts src-lst))))

(defmethod render.get-keywords ((object group) &optional (parameters (request-get-plist)))
  (let* ((name (name object))
         (vendor (getf parameters :vendor))
         (page-name))
    (if vendor
        (setf page-name (format nil "~a ~a" (sklonenie name 1) vendor))
        (setf page-name (format nil "~a" (sklonenie name 1))))
    (format nil "~a, ~a купить, ~a цена, ~a в Санкт-Петербурге, ~a продажа" page-name page-name page-name page-name  page-name)))

(defmethod render.get-keywords ((object product) &optional (parameters (request-get-plist)))
  (declare (ignore parameters))
  (let ((page-name (name-seo object)))
    (format nil "~a, ~a купить, ~a цена, ~a в Санкт-Петербурге, ~a продажа" page-name page-name page-name page-name  page-name)))


(defmethod render.get-title ((object group) &optional (parameters (request-get-plist)))
  (let ((name (name object))
        (vendor (vendor-transform-from-alias (getf parameters :vendor))))
    (string-titlecase
     (if vendor
         (format nil "~a ~a - купить ~a ~a по низкой цене, продажа ~a ~a с доставкой и гарантией в ЦиFры 320-8080"
                 (sklonenie name 1)
                 vendor
                 (sklonenie name 2)
                 vendor
                 (sklonenie name 3)
                 vendor)
         (format nil "~a - купить ~a  по низкой цене, продажа ~a с доставкой и гарантией в ЦиFры 320-8080"
                 (sklonenie name 1)
                 (sklonenie name 2)
                 (sklonenie name 3))))))

(defmethod render.get-description ((object group) &optional (parameters (request-get-plist)))
  (let ((name (name object))
        (vendor (getf parameters :vendor)))
    (string-titlecase
     (if vendor
         (format nil "Купить ~a ~a по низкой цене, продажа ~a ~a с доставкой и гарантией в ЦиFры 320-8080"
                 (sklonenie name 2)
                 vendor
                 (sklonenie name 3)
                 vendor)
         (format nil "Купить ~a  по низкой цене, продажа ~a с доставкой и гарантией в ЦиFры 320-8080"
                 (sklonenie name 2)
                 (sklonenie name 3))))))


(defmethod render.render ((object group) &optional (parameters (request-get-plist)))
  (let ((name (name object))
        (showall (getf parameters :showall)))
    (default-page
        (soy.catalog:content
         (list :name name
               :breadcrumbs (soy.catalog:breadcrumbs (breadcrumbs-add-vendor1 (render.breadcrumbs object) parameters))
               :menu (render.menu object)
               :rightblocks (append
                             (marketing-filters.render-filters object showall)
                             ;;fullfilter
                             (let ((ret (rightblocks object parameters)))
                               (when (fullfilter object)
                                 (push (render.render (fullfilter object) parameters) ret))
                               ret))
               :subcontent  (if (and (null (products object))
                                     (null (getf parameters :fullfilter))
                                     (null (getf parameters :vendor)))
                                ;; Отображаем группы
                                (soy.catalog:centergroup
                                 (list
                                  :producers (when showall
                                               (render.show-producers (storage.get-filtered-products object #'atom)))
                                  :accessories (soy.catalog:accessories)
                                  :groups (let ((sort-groups (sort (remove-if-not #'active (groups object)) #'menu-sort)))
                                            (mapcar #'(lambda (child)
                                                        (let* ((show-func (if showall
                                                                              #'atom
                                                                              #'active))
                                                               (products (storage.get-filtered-products child show-func))
                                                               (filters (marketing-filters.get-filters child products)))
                                                          (list
                                                           :is-active (active child)
                                                           :name (name child)
                                                           :key (key child)
                                                           :cnt (if products
                                                                    (length products)
                                                                    "-")
                                                           :pic (pic child)
                                                           :filters (mapcar #'(lambda (v)
                                                                                (let ((filter (car v))
                                                                                      (num (cdr v)))
                                                                                  (list :name (name filter)
                                                                                        :groupkey (key child)
                                                                                        :key (key filter)
                                                                                        :num (format nil "(~a)" num))))
                                                                            filters))))
                                                    sort-groups))))
                                ;;else
                                (let* ((products-list (if showall
                                                          (storage.get-filtered-products object #'atom)
                                                          (storage.get-filtered-products object #'active)))
                                       (cartrige-group (string= "cartridge-dlya-printerov" (key object)))
                                       (printer-articul (and cartrige-group (getf parameters :printer-articul)))
                                       (storage-printer (when printer-articul
                                                          (getobj printer-articul 'product)))
                                       (cartrige-printer (when printer-articul
                                                           (gethash printer-articul *printer-storage*))))
                                  (unless (getf parameters :sort)
                                    (setf (getf parameters :sort) "pt"))
                                  (awhen (getf parameters :vendor)
                                    (setf products-list
                                          (remove-if-not
                                           #'(lambda (p)
                                               (vendor-filter-controller p (vendor-transform-from-alias it)))
                                           products-list)))
                                  (when (getf parameters :fullfilter)
                                    (setf products-list (fullfilter-controller products-list object parameters)))
                                  (when (and printer-articul cartrige-printer)
                                    (setf products-list (keys-to-objects
                                                         (cartrige.get-cartriges-by-model printer-articul)
                                                         :type 'product)))
                                  (with-sorted-paginator
                                      products-list
                                    parameters
                                    (soy.catalog:centerproduct
                                     (list
                                      :sorts (sorts parameters)
                                      :producers (render.show-producers
                                                  (storage.get-filtered-products
                                                   object
                                                   (if showall #'atom #'active)))
                                      :accessories (soy.catalog:accessories)
                                      :pager pager
                                      :cartrigeselect (when cartrige-group
                                                        (soy.catalog:cartige-select
                                                         (list
                                                          :vendors (cartrige.get-vendors-list)
                                                          :printer (when (and storage-printer
                                                                              cartrige-printer)
                                                                     (soy.catalog:printer-view (list
                                                                                                :articul printer-articul
                                                                                                :pic (car (get-pics printer-articul))
                                                                                                :name (name-seo storage-printer)
                                                                                                :key (key storage-printer)
                                                                                                :price (if (active storage-printer)
                                                                                                           (siteprice storage-printer))))))))
                                      :products
                                      (loop
                                         :for product :in  paginated :collect (render.view product)))))))))
        :keywords (render.get-keywords object parameters)
        :description (render.get-description object parameters)
        :title (render.get-title object parameters))))

(defmethod render.render ((object group-filter) &optional (parameters (request-get-plist)))
  (when (not (equal "" object))
    (soy.fullfilter:container
     (list :name (name object)
           :vendor (getf parameters :vendor)
           :sort (getf parameters :sort)
           :base (format nil "~{~a~}"
                         (mapcar #'(lambda (elt)
                                     (filter-element elt parameters))
                                 (base object)))
           :advanced (when (advanced object)
                       (format nil "~{~a~}"
                               (mapcar #'(lambda (elt)
                                           (soy.fullfilter:group
                                            (list :name (car elt)
                                                  :elts (mapcar #'(lambda (inelt)
                                                                    (filter-element inelt parameters))
                                                                (cadr elt)))))
                                       (advanced object))))
           :isshowadvanced (is-need-to-show-advanced object parameters)))))


(defmethod restas:render-object ((designer eshop-render) (object group-filter))
  (render.render object))

(defun render.gen-uspale-product (prod)
  (when prod
    (let* ((articul (articul prod))
           (name (name-seo prod))
           (group (parent prod))
           (pic (car (get-pics articul)))
           (siteprice (siteprice prod)))
      (list
       ;;TODO: :rating
       ;;TODO: :badge
       :grouplink (key group)
       :groupname (name group)
       :articul articul
       :pic pic
       :name name
       :showsiteprice (get-format-price (siteprice prod))
       :showprice (get-format-price (price prod))
       :buttonaddcart (soy.buttons:add-product-cart (list
                                                     :articul articul
                                                     :name name
                                                     :pic pic
                                                     :siteprice siteprice))))))

(defun render.prepare-upsale-block (name items)
  (list :name name
        :items (mapcar (lambda (item)
                         (render.gen-uspale-product item))
                       items)))


(defun render.get-upsale-products (gr)
  (get-randoms-from-list (remove-if-not #'active (products gr)) 4))


(defmethod render.prepare-upsale-full ((object group))
  (list :groupnameskl (sklonenie (name object) 2)
        :upsaleblocks (mapcar #'(lambda (gr)
                                  (render.prepare-upsale-block (name gr) (render.get-upsale-products gr)))
                              (upsale-links object))))


(defmethod render.view ((object product))
  (let ((pics (get-pics (articul object))))
    (let ((group (parent object)))
      (list :articul (articul object)
            :name (name-seo object)
            :groupname (if (null group)
                           "group not found"
                           (name group))
            :groupkey  (if (null group)
                           ""
                           (key group))
            :price (siteprice object)
            :bonuscount (when (and (bonuscount object)
                                   (plusp (bonuscount object)))
                          (* 10 (bonuscount object)))
            :formatprice (get-format-price (siteprice object))
            :formatstoreprice (get-format-price
                               (+ (siteprice object)
                                  (delta-price object)))
            :bestprice (plusp (delta-price object))
            :groupd (groupd.is-groupd object)
            :groupd_man (groupd.man.is-groupd object)
            :groupd_woman (groupd.woman.is-groupd object)
            :groupd_holiday (groupd.holiday.is-groupd object)
            :firstpic (car pics)
            :promotiontext (concatenate 'string
                                        (get-option object "Secret" "Продающий текст")
                                        " "
                                        (if (zerop (yml.get-product-delivery-price1 object))
                                            " Акция: доставим бесплатно!"
                                            (if (= 100 (yml.get-product-delivery-price1 object))
                                                " Акция: скидка на доставку 70%. Закажи сейчас!"
                                                (if (= 200 (yml.get-product-delivery-price1 object))
                                                    " Акция: скидка на доставку 30%. Закажи сейчас!"
                                                    (if (= 400 (yml.get-product-delivery-price1 object))
                                                        " Акция: скидка на доставку 20. Закажи сейчас!")))))
            :keyopts (render.get-catalog-keyoptions object)
            :oneclickbutton  (unless (preorder object)
                               (soy.buttons:add-one-click (list :articul (articul object))))
            :addbutton (if (preorder object)
                           (soy.buttons:add-predzakaz (list :articul (articul object)))
                           (soy.buttons:add-product-cart (list :articul (articul object)
                                                               :name (name-seo object)
                                                               :pic (if (null pics) nil (car pics))
                                                               :deliveryprice (delivery-price object)
                                                               :siteprice (price object)
                                                               :price (siteprice object))))))))


(defun render.render-optgroups (optgroups)
  (let ((optlist
         (remove-if
          #'null
          (mapcar
           #'(lambda (optgroup)
               ;;не отображать группу опций с именем "Secret"
               (if (string/= (getf optgroup :name)
                             "Secret")
                   (let ((options
                          (mapcar #'(lambda (option)
                                      (unless (equal "" (getf option :value))
                                        (soy.product:option
                                         (list :name (getf option :name)
                                               :value (getf option :value)))))
                                  (getf optgroup :options))))
                     (if (notevery #'null options)
                         (soy.product:optgroup (list :name (getf optgroup :name)
                                                     :options options))
                         ""))))
          optgroups))))
    (when optlist
      (soy.product:optlist (list :optgroups optlist)))))

(defmethod render.get-catalog-keyoptions ((object product))
  (let ((parent (parent object)))
    (when parent
      (remove-if
       #'null
       (mapcar
        #'(lambda (pair)
            (let ((key-optgroup (getf pair :optgroup))
                  (key-optname  (getf pair :optname))
                  (key-alias  (getf pair :showname))
                  (key-units  (getf pair :units))
                  (optvalue))
              (mapcar #'(lambda (optgroup)
                          (when (string= (getf optgroup :name) key-optgroup)
                            (let ((options (getf optgroup :options)))
                              (mapcar #'(lambda (option)
                                          (if (string= (getf option :name) key-optname)
                                              (setf optvalue (getf option :value))))
                                                   options))))
                      (optgroups object))
              (when (valid-string-p optvalue)
                (list :optgroup key-alias
                      :optvalue (if (equal "Есть" optvalue)
                                    ""
                                    optvalue)
                      :optunits key-units))))
        (catalog-keyoptions parent))))))



(defun render.get-keyoptions (product)
  (declare (product product))
  (let ((parent (parent product)))
    (when parent
      (mapcar #'(lambda (pair)
                  (let* ((key-optgroup (getf pair :optgroup))
                         (key-optname  (getf pair :optname))
                         (key-alias  (getf pair :showname))
                         (key-units  (getf pair :units))
                         (optvalue (get-option product key-optgroup key-optname)))
                    (list :optgroup key-optgroup
                          :optname (if (valid-string-p key-alias)
                                       key-alias
                                       key-optname)
                          :optvalue optvalue
                          :optunits key-units)))
              (keyoptions parent)))))

(defmethod render.relink ((object product))
  (let ((temp-rs1)
        (temp-rs2))
    ;;2 случайных товара из списка
    (setf temp-rs1 (get-randoms-from-list
                    ;; список активных товаров той же группы и того же производителя
                    ;; кроме его самого
                    (let ((base-vendor (vendor object)))
                      (remove-if-not
                       #'(lambda (x)
                           (and (not (equal x object))
                                (active x)
                                (equal (vendor object) base-vendor)))
                       (storage.get-filtered-products (parent object))))
                    2))
    ;;4 случайных товара из списка
    (setf temp-rs2 (get-randoms-from-list
                    ;;список всех активных товаров кроме object
                    (let ((all))
                      (process-storage #'(lambda (v)
                                           (when (and (active v)
                                                      (not (equal v object)))
                                             (push v all)))
                                       'product)
                      all)
                    4))
    (loop
       :for item in (append temp-rs1 temp-rs2)
       :for n
       :from 1 to 4
       :collect item)))

(defmethod restas:render-object ((designer eshop-render) (object product))
  (let* ((pics (get-pics (articul object)))
         (diff-percent (servo.diff-percentage (price object) (siteprice object)))
         (is-available (yml.available-for-order-p object))
         (is-vintage (not (or (active object) is-available)))
         (product-view)
         (group (parent object)))
    (setf product-view (list :menu (render.menu object)
                             :breadcrumbs (soy.product:breadcrumbs (render.breadcrumbs object))
                             :articul (articul object)
                             :name (name-seo object)
                             :siteprice (siteprice object)
                             :storeprice (price object)
                             :bestprice (plusp (delta-price object))
                             :groupd (groupd.is-groupd object)
                             :groupd_man (groupd.man.is-groupd object)
                             :groupd_woman (groupd.woman.is-groupd object)
                             :groupd_holiday (groupd.holiday.is-groupd object)
                             :bonuscount (when (and (bonuscount object)
                                                    (plusp (bonuscount object)))
                                           (* 10 (bonuscount object)))
                             :formatsiteprice (get-format-price (siteprice object))
                             :formatstoreprice (get-format-price
                                                (+ (siteprice object)
                                                   (delta-price object)))
                             :equalprice (zerop (delta-price object))
                             :diffprice (delta-price object)
                             :upsaleinfo (when (and group (upsale-links group))
                                           (soy.product:upsale (render.prepare-upsale-full group)))
                             :procent diff-percent
                             :subst (format nil "/~a" (articul object))
                             :pics (cdr pics)
                             :firstpic (when pics (car pics))
                             :optlist (render.render-optgroups (optgroups object))
                             :slogan (concatenate 'string
                                                  (get-option object "Secret" "Продающий текст")
                                                  " "
                                                  (if (zerop (yml.get-product-delivery-price1 object))
                                                      " Акция: доставим бесплатно!"
                                                      (if (= 100 (yml.get-product-delivery-price1 object))
                                                          " Акция: скидка на доставку 70%. Закажи сейчас!"
                                                          (if (= 200 (yml.get-product-delivery-price1 object))
                                                              " Акция: скидка на доставку 30%. Закажи сейчас!"
                                                              (if (= 400 (yml.get-product-delivery-price1 object))
                                                                  " Акция: скидка на доставку 20%. Закажи сейчас!")))))
                             :others (soy.product:others
                                      (list :others (mapcar #'(lambda (x)
                                                                (if (productp x)
                                                                    (render.view x)
                                                                    (list :aricul "0"
                                                                          :name ""
                                                                          :pic "/img/temp/i6.jpg"
                                                                          :price "0"
                                                                          :siteprice "0" :subst ""
                                                                          :firstpic "/img/temp/i6.jpg")))
                                                            (render.relink object))))
                             :keyoptions (filters.limit-end
                                          (remove-if-not #'(lambda (v)
                                                             (valid-string-p
                                                              (getf v :optvalue)))
                                                         (render.get-keyoptions object))
                                          6)
                             :active (active object)
                             :vintage is-vintage
                             :shortdescr (seo-text object)
                             :bestproducts (soy.product:similar-products
                                            (list :products (mapcar #'(lambda (prod)
                                                                        (soy.catalog:product
                                                                         (render.view prod)))
                                                                    (filters.limit-end (servo.find-relative-product-list object) 4))))
                             :seotextflag (valid-string-p (seo-text object))
                             :predzakaz (preorder object)
                             :addproductcart (if (preorder object)
                                                 (soy.buttons:add-predzakaz (list :articul (articul object)))
                                                 (soy.buttons:add-product-cart (list :articul (articul object)
                                                                                     :name (name-seo object)
                                                                                     :pic (if (null pics) nil (car pics))
                                                                                     :siteprice (siteprice object)
                                                                                     :price (price object))))
                             :addoneclick (unless (preorder object)
                                            (soy.buttons:add-one-click (list :articul (articul object)
                                                                             :available is-available)))))
    (default-page
        (soy.product:content product-view)
        :keywords (render.get-keywords object nil)
        :description (format nil "купить ~a в ЦиFры 320-8080 по лучшей цене с доставкой по Санкт-Петербургу"
                             (name-seo object))
        :title (string-titlecase
                (format nil "~a купить в ЦиFры - цена, фотография и описание, продажа ~a с гарантией и доставкой в ЦиFры 320-8080"
                        (name-seo object)
                        (name-seo object))))))


(defun render.make-producters-reverse-lists-by-column (list  &optional (columns 4))
  (let ((len (length list))
        (rs)
        (start-pos 0)
        (segment))
    (multiple-value-bind (division remainder)
        (truncate len columns)
      (loop
         :for i from 1 to columns
         :do (progn
               (setf segment division)
               (when (< 0 remainder)
                 (setf segment (+ 1 segment))
                 (setf remainder (- remainder 1)))
               (push (subseq list start-pos (+ start-pos segment)) rs)
               (setf start-pos (+ start-pos segment)))))
    rs))

(defun render.make-producters-base (list  &key (columns 4) cut)
  (let ((reverse-lists (render.make-producters-reverse-lists-by-column list columns))
        (rs))
    (mapcar #'(lambda (v)
                (let ((len cut))
                  (if (or (null cut)
                          (< (length v) cut))
                      (setf len (length v)))
                  (push (subseq v 0 len) rs)))
            reverse-lists)
    (remove-if #'null rs)))

(defun render.make-producters-hidden (list  &key (columns 4) cut)
  (let ((reverse-lists (render.make-producters-reverse-lists-by-column list columns))
        (rs))
    (mapcar #'(lambda (v)
                (let ((len cut))
                  (if (or (null cut)
                          (< (length v) cut))
                      (setf len (length v)))
                  (push (subseq v len) rs)))
            reverse-lists)
    (remove-if #'null rs)))


(defmethod restas:render-object ((designer eshop-render) (object filter))
  (let* ((request-get-plist (request-get-plist))
         (fltr-name  (getf (data object) :name))
         (group (parent object))
         (group-name (name group))
         (showall (getf request-get-plist :showall))
         (group-children (marketing-filters.group-children group showall))
         (products-list (filters.filter object :obj-set group-children)))
    (if (null (getf request-get-plist :sort))
        (setf (getf request-get-plist :sort) "pt"))
    (awhen (getf (request-get-plist) :vendor)
      (setf products-list
            (remove-if-not #'(lambda (p)
                               (vendor-filter-controller p it))
                           products-list)))
    (with-sorted-paginator
        products-list
      request-get-plist
      (default-page
          (soy.catalog:content
           (list :name fltr-name
                 :breadcrumbs (soy.catalog:breadcrumbs (render.breadcrumbs object))
                 :menu (render.menu object)
                 :rightblocks (append
                               (marketing-filters.render-filters group showall)
                               (rightblocks group request-get-plist))
                 :subcontent (soy.catalog:centerproduct
                              (list
                               :sorts (sorts request-get-plist)
                               :producers (render.show-producers group-children)
                               :accessories (soy.catalog:accessories)
                               :pager pager
                               :products (loop
                                            :for product
                                            :in  paginated
                                            :collect (render.view product))))))
          :keywords (format nil "~a ~a" group-name fltr-name)
          :description (format nil "~a ~a" group-name fltr-name)
          :title (let ((vendor (getf (request-get-plist) :vendor))
                       (name-1 (sklonenie fltr-name 1))
                       (name-2 (sklonenie fltr-name 2))
                       (name-3 (sklonenie fltr-name 3))
                       (grname-1 (sklonenie group-name 1))
                       (grname-2 (sklonenie group-name 2))
                       (grname-3 (sklonenie group-name 3)))
                   (string-titlecase
                    (if vendor
                        (format nil "~a ~a ~a - купить ~a ~a ~a по низкой цене, продажа ~a ~a ~a с доставкой и гарантией в ЦиFры 320-8080"
                                grname-1
                                name-1
                                vendor
                                grname-2
                                name-2
                                vendor
                                grname-3
                                name-3
                                vendor)
                        (format nil "~a ~a - купить ~a ~a  по низкой цене, продажа ~a ~a с доставкой и гарантией в ЦиFры 320-8080"
                                grname-1
                                name-1
                                grname-2
                                name-2
                                grname-3
                                name-3))))))))



(defun render.show-producers (products)
  (declare (list products))
  (let* ((vendors (storage.get-vendors products))
         (url-parameters (request-get-plist))
         (veiws nil))
    (remf url-parameters :page)
    (maphash #'(lambda (k x)
                 (let* ((vendor-alias (awhen (getobj (string-downcase k) 'vendor)
                                             (alias it)))
                        (vendor-url (if (valid-string-p vendor-alias)
                                        vendor-alias
                                        k)))
                   (setf (getf url-parameters :vendor) (hunchentoot:url-encode vendor-url))
                   (push (list :vendor k
                               :cnt x
                               :link (format nil "?~a" (servo.make-get-str url-parameters)))
                         veiws)))
             vendors)
    (setf veiws (sort veiws #'string<= :key #'(lambda (v) (string-upcase (getf v :vendor)))))
    (soy.catalog:producers
     (list
      :vendorblocks (render.make-producters-base
                     veiws :cut 3)
      :vendorhiddenblocks (render.make-producters-hidden
                           veiws :cut 3)))))


(defmethod restas:render-object ((designer eshop-render) (object group))
  (render.render object))

;;;; gateway.lisp

(in-package #:eshop)

(defclass gateway.dump ()
  ((products-num :initform 0 :accessor product-num)
   (is-loaded :initform nil :accessor is-loaded)
   (date :initform 0 :accessor date))
  (:documentation "Information about last product import dump from ERP (or another place)"))

(defclass gateway.erp-data-dump (gateway.dump)
  ((list-raw-data :initform nil :accessor list-raw-data))
  (:documentation "Raw data list of import products from ERP (or another place)"))

(defvar *gateway.loaded-dump* (make-instance 'gateway.dump)
 "Information about last loaded product dump")

(defvar *gateway.dump* (make-instance 'gateway.erp-data-dump)
  "Information about current import from ERP (or another place)")

(defun %gateway.clear-dump (&optional (dump *gateway.dump*))
  "Init dump with new class gateway.erp-data-dump instance"
  (declare (gateway.erp-data-dump dump))
  (setf dump (make-instance 'gateway.erp-data-dump)))

(defun %gateway.add-data->dump (data &optional (dump *gateway.dump*))
  "Add raw ERP products data to dump"
  (declare (gateway.erp-data-dump dump))
  (push data (list-raw-data dump)))

(defun %gateway.processing-fist-package (data &optional (dump *gateway.dump*))
  "Prepare dump, start to collect packet data & give back answer"
  (declare (gateway.erp-data-dump dump))
  (%gateway.clear-dump)
  (%gateway.add-data->dump data dump)
  "first")

(defun %gateway.processing-package (data &optional (dump *gateway.dump*))
  "Collect packet data & give back answer"
  (declare (gateway.erp-data-dump dump))
  (%gateway.add-data->dump data dump)
  "ordinal")

(defun gateway.make-dump-pathname (&optional (timestamp (get-universal-time)))
  "Create pathname dump file (named yyyy-mm-dd_HH:MM:SS.bkp) uses config :PATHS :path-to-gateway"
  (declare (number timestamp))
  (let* ((filename (concatenate 'string (time.encode.backup timestamp) ".bkp"))
         (dump-path (config.get-option "PATHS" "path-to-gateway")))
    (merge-pathnames filename dump-path)))

(defun %gateway.get-time-from-dumpfile (dump-name)
  "Extract time from dump file name (named yyyy-mm-dd_HH:MM:SS.bkp)"
  (declare (pathname dump-name))
  (time.decode.backup (pathname-name dump-name)))

(defun %gateway.search-dump-path-and-timestamp (&optional (timestamp (get-universal-time)))
  "Looking for dump file named with ts near timestamp & give back (values-list filename ts) (NIL NIL) for empty dump-path"
  (declare (number timestamp))
  (let* ((dump-path (config.get-option "PATHS" "path-to-gateway"))
         (dump-mask (concatenate 'string (namestring dump-path) "*.bkp"))
         (current-filename (gateway.make-dump-pathname timestamp))
         (result-file nil)
         (result-time nil))
    (flet ((pathname> (pathname1 pathname2 &key (start1 0) end1 (start2 0) end2)
             (string> (namestring pathname1) (namestring pathname2)
                      :start1 start1 :start2 start2 :end1 end1 :end2 end2)))
      (setf result-file (first (sort (remove-if (rcurry #'pathname> current-filename)
                                                (directory dump-mask)) #'pathname>)))
      (awhen result-file
           (setf result-time (%gateway.get-time-from-dumpfile it)))
      (values-list (list result-file result-time)))))

(defun %gateway.prepare-raw-data (raw)
  "Сonvert raw data to presentable format"
  (slots.string-delete-newlines
   (sb-ext:octets-to-string raw :external-format :cp1251)))

(defun gateway.flush-dump (&optional (dump *gateway.dump*) (timestamp (get-universal-time)))
  "Flush dump data list to file"
  (declare (gateway.erp-data-dump dump) (number timestamp))
  (with-open-file (file (gateway.make-dump-pathname timestamp)
                        :direction :output
                        :if-exists :supersede
                        :if-does-not-exist :create
                        :external-format :utf-8)
    (mapcar #'(lambda (data)
                (format file "~&~A~%"
                        (%gateway.prepare-raw-data data))))
    (reverse (list-raw-data dump))))

(defun %gateway.decode-json-from-file (pathname)
  "Read file and decode json data to appended list"
  (declare (pathname pathname))
  (let ((result))
    (with-open-file (file pathname)
      (loop
         :for i :from 0
         :for line = (read-line file nil 'EOF)
         :until (eq line 'EOF)
         :do (let ((data (json:decode-json-from-string line)))
                 (if (zerop i)
                     (setf result data)
                     (nconc result data)))))
    result))

 (defun %product-update-name (product name)
  "Update product field name"
  (declare (product product) ((or string t) name))
  (when (and name
             (string= "" (name-provider product)))
    (setf (name-provider product) name))
  (when (string= "" (name-seo product))
    (setf (name-seo product) name)))

(defun %product-update-prices (product siteprice price oldprice)
  "Update product fields delta-price & siteprice"
  (declare (product product) ((or number t) siteprice price))
  ;; пересчет дельты если пришла только цена сайта
  (when (and siteprice
             (not price))
    (setf (delta-price product) (- (price product) siteprice)))
  ;; цены
  (when siteprice
    (setf (siteprice product) siteprice))
  ;; перещет дельты не проводится для акционных товаров
  (when (and price
             (not (groupd.is-groupd product)))
    (setf (delta-price product) (- price (siteprice product))))
  ;; старая цена для аукционных товаров
  ;; когда розничная цена и цена сайта совпадают, но необходимо
  ;; отображать разницу
  (when (and (groupd.is-groupd product)
             oldprice
             (zerop (delta-price product))
             (plusp oldprice))
    (setf (delta-price product) (- oldprice (siteprice product)))))

(defun %product-update-bonuscount (product bonuscount)
  "Update product fields bonuscount"
  (declare (product product) ((or number t) bonuscount))
  ;;TODO:(возможно нужно пересчитывать когда приходит новая цена)
  (when bonuscount
    (setf (bonuscount product) bonuscount)))

(defun %product-update-counts (product count-total count-transit)
  "Update product fields count-total count-transit"
  (declare (product product) ((or number t) count-total count-transit))
  (aif count-total
       (setf (count-total product) it)
       (if (and count-transit
                (zerop count-transit)
                (= (count-total product)
                   (count-transit product)))
           0))
  (when count-transit
    (setf (count-transit  product) count-transit)))

(defun %gateway.process-product (item)
  "Process product item plist. Check fields and update data in storage."
  (labels ((@ (field) (getf item field))
           (fl@ (field) (float-string->int (@ field))))
    (awhen (@ :id)
      (let* ((old-product (getobj it 'product))
             (product))
        (setf product (aif old-product
                           it
                           (make-instance 'product :articul (@ :id))))
        (setf (key product) (@ :id))
        (setf (articul product) (fl@ :id))
        (%product-update-name product (@ :name))
        (%product-update-prices product
                                (fl@ :price--site) (fl@ :price) (fl@ :price-old))
        (%product-update-bonuscount product (fl@ :bonuscount))
        (%product-update-counts product (fl@ :count--total) (fl@ :count--transit))
        (setf (active product) (plusp (count-total product)))
        (unless old-product
          (setobj (key product) product))))))

(defun %gateway.process-products-dump-data (items)
  "Process list items. Where items a alist example: ((:ID . \"158354\") (:NAME . \"USB HUB 4port mobileData HB-65\") (:ISNEW . \"0\"))"
  (loop :for item  :in items
     :do (%gateway.process-product (servo.alist-to-plist item))))

(defun %gateway.update-actives (items)
  "Update actives for products not entered the itmes"
  (let ((articuls (make-hash-table :test #'equal)))
    (mapcar #'(lambda (v)
                (let ((articul (cdr (assoc :id v))))
                  (setf (gethash articul articuls) t)))
            items)
    (process-storage #'(lambda (v)
                         (when (and (not (gethash (key v) articuls))
                                    (active v))
                           (setf (active v) nil)
                           (setf (count-total v) 0)
                           (setf (count-transit v) 0)))
                     'product)))

(defun %gateway.singles-pathname ()
  (merge-pathnames "singles.txt" (config.get-option "PATHS" "path-to-gateway")))

(defun gateway.restore-singles (dump-timestamp &optional (current-timestamp (get-universal-time)))
  "Load singles products witch came between dump-timestamp and current-timestamp"
  (declare (number dump-timestamp current-timestamp))
  (let* ((data))
    (labels ((@time (line) (subseq line 0 19))
             (@json (line) (subseq line 21))
             (@validp (line) (<= dump-timestamp (time.decode.backup (@time line))
                                current-timestamp)))
      (with-open-file (file (%gateway.singles-pathname))
        (loop
           :for line = (read-line file nil 'EOF)
           :until (eq line 'EOF)
           :when (@validp line)
           :do (progn
                 (log5:log-for info-console "single: ~A" line)
                 (setf data (json:decode-json-from-string (@json line)))
                 (%gateway.process-products-dump-data data)))))))

(defun %gateway.load-data (last-dump last-dump-ts &optional (timestamp (get-universal-time)))
  "Load product data from decoded alists"
  (declare (number last-dump-ts timestamp))
   (let ((data (%gateway.decode-json-from-file last-dump)))
    (setf (date *gateway.loaded-dump*) last-dump-ts)
    (setf (product-num *gateway.loaded-dump*) (length data))
    (%gateway.process-products-dump-data data)
    (%gateway.update-actives data)
    (gateway.restore-singles last-dump-ts timestamp)
    (post-proccess-gateway)))

(defun %gateway.load-dump (&optional (dump *gateway.dump*))
  "Load products from dump"
  (declare (gateway.erp-data-dump dump))
  (let ((last-dump)
         (last-dump-ts (date dump)))
    (loop
       :for i :from 0
       :for line :in (list-raw-data dump)
       :do (let ((data (json:decode-json-from-string line)))
             (if (zerop i)
                 (setf last-dump data)
                 (nconc last-dump data))))
    (%gateway.load-data last-dump last-dump-ts)))

(defun gateway.load (&optional (timestamp (get-universal-time)))
  "Load products from file dump"
  (declare (number timestamp))
  (multiple-value-bind (last-dump last-dump-ts) (%gateway.search-dump-path-and-timestamp timestamp)
    (awhen last-dump
      (%gateway.load-data last-dump last-dump-ts timestamp))))

(defun %gateway.store-and-processed-dump (&optional (dump *gateway.dump*))
  "Store data, load and processed data"
  (declare (gateway.erp-data-dump dump))
  (gateway.flush-dump dump)
  (%gateway.load-dump dump))

(defun %gateway.processing-last-package (data &optional (dump *gateway.dump*))
  "Finish data collecting, start separate proccess to store and load dump give back answer"
  (%gateway.add-data->dump data dump)
  (setf (date dump) (get-universal-time))
  (bt:make-thread #L(%gateway.store-and-processed-dump dump)
                  :name "store-and-processed-dump")
  (%gateway.clear-dump)
  "last")

(defun gateway.store-single-gateway (data &optional (timestamp (get-universal-time)))
  "Сохраняет одиночные выгрузки в файл"
  (with-open-file (file (%gateway.singles-pathname)
                        :direction :output
                        :if-exists :append
                        :if-does-not-exist :create
                        :external-format :utf-8)
    (format file "~&~A>>~A~%" (time.encode.backup timestamp) data)))

(defun %gateway.processing-single-package (raw)
  "Обработка одиночного изменения, для экстренного внесения изменений на небольшое количество товаров"
  (let ((data (%gateway.prepare-raw-data raw)))
    (gateway.store-singles data)
    (%gateway.process-products-dump-data (json:decode-json-from-string data))
    ;; возможно тут необходимо пересчитать списки активных товаров или еще что-то
    "single"))

(defun gateway-page ()
  "GP"
  (setf (hunchentoot:content-type*) "text/html; charset=utf-8")
  (let ((raw (hunchentoot:raw-post-data))
        (num (hunchentoot:get-parameter "num"))
        (single (hunchentoot:get-parameter "single")))
    (aif raw
         (string-case num
           ("1" (%gateway.processing-fist-package raw))
           ("0" (%gateway.processing-last-package raw))
           (t (string-case single
                ("single" (%gateway.processing-single-package raw))
                (t (%gateway.processing-package raw)))))
        "NIL")))

;;;; gateway.lisp

(in-package #:eshop)

(defparameter *single-history* nil)

(defparameter *load-list* nil)
(defparameter *order* nil)
(defparameter *serialize-check-flag* t)

(defun gateway-page ()
  (setf (hunchentoot:content-type*) "text/html; charset=utf-8")
  (let ((raw (hunchentoot:raw-post-data)))
    (if (null raw)
        "NIL"
        (progn
          (cond ((string= "0" (hunchentoot:get-parameter "num"))
                 ;; Обработка последнего пакета
                 (progn
                   ;; Делаем все продукты неактивными
                   (push raw *load-list*)
                   (push (hunchentoot:get-parameter "num") *order*)
                   ;; Обрабатываем все сохраненные пакеты
                   (let ((data))
                     (loop :for packet :in (reverse *load-list*)
                        :do (setf data (append (json:decode-json-from-string
                                                (sb-ext:octets-to-string packet :external-format :cp1251)) data))))
                   ;;создаем новый yml файл
                   ;;(create-yml-file)
                   ;; Заполняем siteprice если он равен 0
                   ;; (copy-price-to-siteprice)
                   ;; Сохраняем *load-list* и *order* для истории
                   (gateway.store-history (list (list (time.get-date-time) *order* *load-list*)))
                   ;; Обнуляем *load-list* и *order* (если приходит 1 пакет, то он num=0)
                   (post-proccess-gateway)
                   (setf *load-list* nil)
                   (setf *order* nil)
                   "last"))
                ((string= "1" (hunchentoot:get-parameter "num"))
                 ;; Обработка первого пакета
                 (progn
                   ;; Обнуляем *load-list* и *order*
                   (setf *load-list* nil)
                   (setf *order* nil)
                   ;; Засылаем первый пакет в *load-list*
                   (push raw *load-list*)
                   (push (hunchentoot:get-parameter "num") *order*)
                   "first"))
                ((string= "1" (hunchentoot:get-parameter "single"))
                 ;; Обработка одиночного изменения, для экстренного внесения изменений на небольшое количество товаров
                 (progn
                   (let ((name (hunchentoot:get-parameter "user")))
                     (log5:log-for info "GATEWAY::Single")
                     ;; сохраняем запрос
                     (gateway.store-singles (list (list (time.encode.backup) name raw)))
                     ;; обрабатываем данные пришедшие в одиночном запросе
                     (gateway.restore-singles1 (gateway.get-last-gateway-ts) (get-universal-time))
                     ;; возможно тут необходимо пересчитать списки активных товаров или еще что-то
                     "single")))
                (t
                 ;; Обработка промежуточных пакетов
                 (progn
                   ;; Засылаем первый пакет в *load-list*
                   (push raw *load-list*)
                   (push (hunchentoot:get-parameter "num") *order*)
                   "ordinal")))))))


(defun gateway.check-price (product price siteprice)
  (let ((price-old (+ (siteprice product) (delta-price product)))
        (siteprice-old (siteprice product))
        (mailbody))
    (if (or (and (< 3000 siteprice-old)
                 (<= 0.2 (float (/ (abs (- siteprice-old siteprice 1)) siteprice-old))))
            (and (< 3000 price-old)
                 (<= 0.2 (float (/ (abs (- price-old price 1)) price-old)))))
        (progn
          (setf mailbody (format nil "~&<a href=\"http://www.320-8080.ru/~a\">~a: ~a</a>
                                         <br/>Изменение цены боллее чем на 20%
                                         <br/>Старая цене на сайте:~a |  новая:~a
                                         <br/>Разница в цене в магазине:~a | новая:~a"
                                 (articul product)
                                 (articul product)
                                 (name-provider product)
                                 siteprice-old siteprice
                                 price-old price))
          (if *serialize-check-flag*
              (progn
                (setf (delta-price product) (if (zerop siteprice)
                                                0
                                                (- price siteprice)))
                (setf (siteprice product) (if (zerop siteprice)
                                              price
                                              siteprice))
                (mapcar #'(lambda (email)
                            (email.send-mail-warn (list email) mailbody (format nil "price ~a" (articul product))))
                        *conf.emails.gateway.warn*)))))
    t))

(defun gateway.process-product1 (articul raw-price raw-siteprice isnew isspec
                                 name realname raw-count-total raw-count-transit
                                 raw-bonuscount raw-oldprice)
  (declare (ignore isnew isspec))
  (let* ((old-product (getobj (format nil "~a" articul) 'product))
         (product (aif old-product
                       it
                       (make-instance 'product :articul articul)))
         (price (ceiling (arnesi:parse-float raw-price)))
         (siteprice (ceiling (arnesi:parse-float raw-siteprice)))
         (oldprice (ceiling (arnesi:parse-float raw-oldprice)))
         (count-total (ceiling (arnesi:parse-float raw-count-total)))
         (count-transit (ceiling (arnesi:parse-float raw-count-transit)))
         (bonuscount (ceiling (arnesi:parse-float raw-bonuscount))))
    ;; ключ строка
    (setf (key product) (format nil "~a" articul))
    ;; артикул число
    (setf (articul product) articul)
    ;; имена
    ;; имя из 1С
    (when (and name
               (equal "" (name-provider product)))
      (setf (name-provider product)  name))
    (if realname
        (if (equal "" (name-seo product))
            (setf (name-seo product) realname))
        (if (equal "" (name-seo product))
            (setf (name-seo product) name)))
    ;; пересчет дельты если пришла только цена сайта
    (if (and raw-siteprice
             (not raw-price))
        (setf (delta-price product) (- (price product) siteprice)))
    ;; цены
    (if raw-siteprice
        (setf (siteprice product) siteprice))
    (if raw-price
        (setf (delta-price product) (- price (siteprice product))))
    ;; бонусы (нужно пересчитывать когда приходит новая цена)
    (if raw-bonuscount
        (setf (bonuscount product) bonuscount))
    ;; количество
    (if raw-count-total
        (setf (count-total product) count-total)
        (if (and raw-count-transit
                 (equal 0 count-transit)
                 (equal (count-total product)
                        (count-transit product)))
            0))
    ;; старая цена для аукционных товаров
    ;; когда розничная цена и цена сайта совпадают, но необходимо
    ;; отображать разницу
    (when (and (groupd.is-groupd product)
             raw-oldprice
             (= (delta-price product) 0)
             (/= oldprice 0))
        (setf (delta-price product) (- oldprice (siteprice product)))
        (format t "~&~a:~a | ~a ~a ~a" articul name price oldprice raw-oldprice))
    (if raw-count-transit
        (setf (count-transit  product) count-transit))
    ;; проставляем флаг active
    (setf (active product) (plusp (count-total product)))
    (unless old-product
      (setobj (key product) product))))


(defun gateway.store-single-gateway (raws &optional (timestamp (get-universal-time)))
  "Сохраняет одиночные выгрузки в файл"
  (let ((filename (time.encode.backup timestamp))
        (pathname (format nil "~a/gateway/singles.txt" *path-to-logs*)))
    (with-open-file (file pathname
                          :direction :output
                          :if-exists :append
                          :if-does-not-exist :create
                          :external-format :utf-8)
      (format file "~a>>~a~%" filename raws))))

(defun gateway.store-full-gateway (raws &optional (timestamp (get-universal-time)))
  (let* ((filename (format nil "~a.bkp" (time.encode.backup timestamp)))
         (pathname (merge-pathnames filename (config.get-option "PATHS" "path-to-gateway"))))
    (with-open-file (file pathname
                          :direction :output
                          :if-exists :supersede
                          :if-does-not-exist :create
                          :external-format :utf-8)
      (mapcar #'(lambda (data)
                  (format file "~&~a" (object-fields.string-delete-newlines (sb-ext:octets-to-string data :external-format :cp1251))))
              (reverse raws)))))

(defun gateway.store-history (history)
  "history имеет вид (list (list (time.get-date-time) *order* *load-list*))"
  (mapcar #'(lambda (v)
              (log5:log-for info-console "~a|~a|~a" (car v) (second v) (length (third v)))
              (gateway.store-full-gateway (third v) (time.decode-gateway-time (car v))))
          history))

(defun gateway.process-products1 (items)
  (loop :for elt  :in items :collect
     (block iteration
       (let ((articul   (ceiling (arnesi:parse-float (cdr (assoc :id elt)))))
             (price     (cdr (assoc :price elt)))
             (siteprice (cdr (assoc :price--site elt)))
             (bonuscount (cdr (assoc :bonuscount elt)))
             (isnew     (cdr (assoc :isnew  elt)))
             (isspec    (cdr (assoc :isspec elt)))
             (name      (cdr (assoc :name elt)))
             (realname  (cdr (assoc :realname elt)))
             (oldprice (cdr (assoc :price--old elt)))
             (count-total    (cdr (assoc :count--total elt)))
             (count-transit  (cdr (assoc :count--transit elt))))
         ;; (log5:log-for info-console "~a" elt)
         (gateway.process-product1 articul price siteprice isnew isspec name realname count-total count-transit bonuscount oldprice)))))

(defun gateway.get-pathname-fulls (&optional (timestamp (get-universal-time)))
  "список файлов выгрузок до определенной даты"
  (let* ((filename (time.encode.backup timestamp))
         (current-name (format nil "~a/gateway/~a.bkp" *path-to-logs* filename)))
    (remove-if #'(lambda (v) (string< current-name (format nil "~a" v)))
               (reverse (directory
                         (format nil "~a/gateway/*.bkp" *path-to-logs*))))))

(defun gateway.get-pathname-last-full (&optional (timestamp (get-universal-time)))
  "имя файла последней выгрузки относительно метки времени или текущей даты"
  (car (gateway.get-pathname-fulls timestamp)))

(defun gateway.restore-singles1 (gateway-timestamp &optional (current-timestamp (get-universal-time)))
  "список одиночных выгрузок"
  (let ((filename (format nil "~a/gateway/singles.txt" *path-to-logs*))
        (start (time.encode.backup gateway-timestamp))
        (finish (time.encode.backup current-timestamp))
        (stop nil)
        (data))
    (with-open-file (file filename)
      (loop
         :for line = (read-line file nil 'EOF)
         :until (or (eq line 'EOF)
                    stop)
         :do (progn
               (when (and (string<= start (subseq line 0 19))
                          (string<= (subseq line 0 19) finish))
                 (log5:log-for info "single: ~a" line)
                 (setf data (json:decode-json-from-string (subseq line 21)))
                 (gateway.process-products1 data)))))))



(defun gateway.update-actives (data)
  ;; TOCHECK
  (let ((articuls (make-hash-table :test #'equal)))
    (mapcar #'(lambda (v)
                (let ((articul (format nil "~a" (cdr (assoc :id v)))))
                  (setf articul (ceiling (arnesi:parse-float articul)))
                  (setf articul (format nil "~a" articul))
                  (setf (gethash articul articuls) t)))
            data)
    (process-storage #'(lambda (v)
                         (when (and (not (gethash (key v) articuls))
                                    (active v))
                           (setf (active v) nil)
                           (setf (count-total v) 0)
                           (setf (count-transit v) 0)))
                     'product)))

(defun gateway.get-last-gateway-ts (&optional (timestamp (get-universal-time)))
  (let* ((filename (time.encode.backup timestamp))
         (current-name (format nil "~a/gateway/~a.bkp" *path-to-logs* filename))
         (last-gateway))
    (setf last-gateway (car
                        (remove-if #'(lambda (v) (string< current-name (format nil "~a" v)))
                                   (reverse (directory
                                             (format nil "~a/gateway/*.bkp" *path-to-logs*))))))
    (if last-gateway
        (time.decode.backup
         (subseq
          (car (last (split-sequence:split-sequence
                      #\/
                      (format nil "~a" last-gateway)))) 0 19))
        timestamp)))

(defun gateway.restore-history (&optional (timestamp (get-universal-time)))
  (let* ((filename (time.encode.backup timestamp))
         (current-name (format nil "~a/gateway/~a.bkp" *path-to-logs* filename))
         (*serialize-check-flag* nil)
         (last-gateway))
    (log5:log-for info "gateway current time ~a" current-name)
    (setf last-gateway (car
                        (remove-if #'(lambda (v) (string< current-name (format nil "~a" v)))
                                   (reverse (directory
                                             (format nil "~a/gateway/*.bkp" *path-to-logs*))))))
    (log5:log-for info "gateway restore file ~a" last-gateway)
    (if last-gateway
        (let ((data)
              (lastgateway-ts (time.decode.backup
                               (subseq
                                (car (last (split-sequence:split-sequence
                                            #\/
                                            (format nil "~a" last-gateway)))) 0 19))))
          (with-open-file (file last-gateway)
            (loop
               :for line = (read-line file nil 'EOF)
               :until (eq line 'EOF)
               :do (setf data (append (json:decode-json-from-string line) data))))
          (gateway.process-products1 data)
          (gateway.update-actives data)
          (gateway.restore-singles1 lastgateway-ts timestamp)
          (post-proccess-gateway))))
  (log5:log-for info "gateway restore finished")
  "done")

(defun gateway.store-singles (history)
  (mapcar #'(lambda (v)
              (gateway.store-single-gateway (object-fields.string-delete-newlines (sb-ext:octets-to-string (third v) :external-format :cp1251)) (time.decode.backup (car v))))
          history))

;;;; search.lisp

(in-package #:eshop)

(defun search-engine (q size)
  (let* ((wordlist (mapcar #'string-downcase (split-sequence #\Space q)))
         (results (make-hash-table :test #'equal))
         (result-list nil)
         (sorted-list nil))
    (maphash #'(lambda (key val)
                 (when (and (typep val 'product)
                            (active val)
                            (new-classes.parent val))
                   (let ((name (string-downcase (format nil "~a" (name-seo val)))))
                     (mapcar #'(lambda (word)
                                 (let* ((search-result (search word name))
                                        (tmp (gethash key results (list :name name :rel 0))))
                                   (when search-result
                                     (setf (getf tmp :rel) (+ 1 (getf tmp :rel)))
                                     (setf (gethash key results) tmp))))
                             wordlist))))
             (storage *global-storage*))
    ;; Преобразуем хэш с элементами вида (list :name "xxx" :rel "yyy") в список для сортировки
    (maphash #'(lambda (key val)
                 (push (list* :id key val) result-list))
             results)
    ;; Сортируем список
    (setf sorted-list (sort result-list #'(lambda (a b) (> (getf a :rel) (getf b :rel)))))
    ;; Обрезаем результаты
    (when (< size (length sorted-list))
      (setf sorted-list (subseq sorted-list 0 size)))
    ;; Возвращаем список продуктов
    (mapcar #'(lambda (x)
                (gethash (getf x :id) (storage *global-storage*)))
            sorted-list)))


(defun get-match-products (q)
  (when (string/= q "")
    (let ((articul (parse-integer q :junk-allowed t)))
      (if (null articul)
          (search-engine q 50)
          ;; else
          (let ((result (gethash (format nil "~a" articul) (storage *global-storage*))))
            (if (null result)
                (search-engine q 50)
                (list result)))))))

(defun get-safe-url-decode-value (param)
  param)


(defun search-page ()
  (let* ((q (hunchentoot:get-parameter "q"))
         (search-string (strip q))
         (url-decoded (get-safe-url-decode-value search-string)))
    (cond ((string= q "")              (make-output "Введите поисковый запрос!"))
          ((null url-decoded)          (make-output))
          ((> 3 (length url-decoded))  (make-output "Слишком короткий поисковый запрос"))
          (t  (let ((search-result (get-match-products (strip url-decoded))))
                (if (null search-result)
                    (make-output)
                    (make-output (prefer search-result))))))))

(defun prefer (products)
  (soy.catalog:centerproduct
   (list
    ;; :producers (group:make-vendor-filter (parent object))
    :accessories (soy.catalog:accessories)
    :products (loop
                 :for product
                 :in (remove-if #'(lambda (x)
                                    (null (active x)))
                                products)
                 :collect (render.view product)))))



(defun make-output (&optional (centercontent nil))
  (default-page
      (soy.catalog:content
       (list :name "Поиск мысли..."
             :breadcrumbs "<a href=\"/catalog\">Каталог</a> / Поиск"
             :menu (new-classes.menu)
             :rightblocks (list (soy.catalog:rightblock1)
                                (soy.catalog:rightblock2))
             :subcontent (if (null centercontent)
                             "Ничего не найдено"
                             (format nil "~a" centercontent))))))


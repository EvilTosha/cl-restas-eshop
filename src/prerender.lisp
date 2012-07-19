;;;; prerender.lisp
;;; set of methods for converting templates in articles to products views,
;;; images, buttons, etc
;;; teplates look like <!--#(temp_name);arg1;arg2;...;-->

(in-package #:eshop)

;;составление строки по данным аргументам
(defun prerender-args-to-html (args)
  (let* ((type (string-trim '(#\Space) (nth 0 args))))
    (cond
      ;;вставка картинки
      ((string= type "pic")
       (let* ((size (string-trim '(#\Space) (nth 1 args)))
              (articul (string-trim '(#\Space) (nth 2 args)))
              (path-art  (ppcre:regex-replace  "(\\d{1,3})(\\d{0,})"
                                               (format nil "~a" articul)
                                               "\\1/\\1\\2" ))
              (number (- (parse-integer
                          (string-trim '(#\Space) (nth 3 args))) 1))
              (product (getobj articul 'product))
              (picname (nth number (get-pics articul)))
              (height (nth 5 args))
              (width (nth 4 args))
              (style ""))
         (when height
           (setf style (format nil "~aheight:~apx;" style
                               (string-trim '(#\Space) height))))
         (when width
           (setf style (format nil "~awidth:~apx;" style
                               (string-trim '(#\Space) width))))
         (when (and (not picname) (get-pics articul))
           (setf picname (car (get-pics articul))))
         (when (and product (name-seo product) picname)
           (let* ((path (format nil "~a/~a/~a" size articul picname))
                  (path* (format nil "~a/~a/~a" size path-art picname)))
             (when (and (not height) (not width))
               (multiple-value-bind
                     (width height)
                   (pics:get-dimensions (format nil "~a/~a" (config.get-option "PATHS" "path-to-pics") path*))
                 (setf style (pics:style-for-resize width height 600))))
             (format nil "<a href=\"/~a\" title=\"~a\">~%
                                   <img src=\"/pic/~a\" alt=\"~a\" style=\"~a\"/>~%
                                </a>~%"
                     articul (name-seo product) path (name-seo product) style)))))
      ;;вставка области для маппинга
      ((string= type "area")
       (let* ((c1 (nth 1 args))
              (c2 (nth 2 args))
              (c3 (nth 3 args))
              (c4 (nth 4 args))
              (articul (nth 5 args))
              (product (getobj articul 'product))
              (name (name-seo product))
              (siteprice (siteprice product))
              ;; (delivery-price (delivery-price product))
              (picname (car (get-pics articul))))
         (format nil "<area shape=\"rect\" coords=\"~a,~a,~a,~a\"
                     href=\"#add\" ~a>"
                 c1 c2 c3 c4
                 (soy.buttons:add-product-onclick
                  (list :articul articul
                        :name name
                        :siteprice siteprice
                        :pic picname)))))
      ;;вставка кнопки покупки
      ((string= type "buy")
       (let* ((articul (nth 1 args))
              (product (getobj articul 'product)))
         (when product
           (let ((name (name-seo product))
                 (siteprice (siteprice product))
                 (picname (car (get-pics articul))))
             (format nil "<span class=\"add\" id=\"add-img\"><big class=\"price\"><b>~a</b><var> руб.</var></big>~a"
                     (get-format-price siteprice)
                     (soy.buttons:add-product-cart
                      (list :articul articul
                            :name name
                            :siteprice siteprice
                            :pic picname)))))))
      ;;вставка нескольких акционнных товаров
      ((string= type "rasprodaja")
       (let* ((articul (nth 1 args))
              (product (getobj articul 'product)))
         (when product
           (let ((name (name-seo product))
                 (siteprice (siteprice product))
                 (picname (car (get-pics articul))))
             (format nil "<b><a href=\"\\~a\">~a</a></b><br><br>
<span class=\"add\" id=\"add-img\">
                           <big class=\"strike price\">~a</big><var> руб.</var><br>
                           <big class=\"price red\"><b>~a</b></big><var class=\"red\"> руб.</var><br>
~a"
                     articul
                     (name-seo product)
                     (get-format-price (price product))
                     (get-format-price siteprice)
                     (soy.buttons:add-product-rasp
                      (list :articul articul
                            :name name
                            :siteprice siteprice
                            :pic picname)))))))
      ((string= type "price")
       (let* ((articul (nth 1 args))
              (product (getobj articul 'product)))
         (when product
           (let ((siteprice (siteprice product)))
             (format nil "~a"
                     (get-format-price siteprice))))))
      (t
       (format nil "<!-- unknown format -->~%")))))

(defun prerender-string-replace (string)
  (let* ((start (search "<!--#" string)) (end (search ";-->" string)))
    (if (null start)
        string
        (concatenate 'string (subseq string 0 start)
                     (prerender-args-to-html
                      (split-sequence #\;
                                      (subseq string (+ 5 start) (+ 1 end))
                                      :remove-empty-subseqs t))
                     (prerender-string-replace (subseq string (+ 4 end)))))))

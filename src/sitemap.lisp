;;;; sitemap.lisp

(in-package #:eshop)

;; example:
;; <loc>http://www.example.com/</loc>
;; <lastmod>2005-01-01</lastmod>
;; <changefreq>monthly</changefreq>
;; <priority>0.8</priority>

(defparameter *sitemap-lastmod-time* nil)
(defparameter *sitemap-num-routs* nil)
(defparameter *sitemap-routs-storage* nil)
(defparameter *sitemap-stream* nil)

(defun sitemap.get-item-route (item &key (lastmod *sitemap-lastmod-time*)
                               (changefreq "daily") (priority "0.5"))
  ;; TODO: get rid of explicit url (move to global var?)
  ;; TOCHECK
  (list :loc (format nil "http://www.320-8080.ru/~a" (hunchentoot:url-encode (key item)))
        :lastmod lastmod
        :changefreq changefreq
        :priority priority))

(defun sitemap.get-groups-routes ()
  (process-and-collect-storage
   'group :func #'sitemap.get-item-route
   :when-func #'(lambda (group)
                  (and group (active group)
                       (not (empty group))))))

(defun sitemap.get-products-routes ()
  (process-and-collect-storage
   'product :func #'sitemap.get-item-route
   :when-func #'active))

(defun sitemap.get-filters-routes ()
  (process-and-collect-storage 'filter :func #'sitemap.get-item-route))

(defun sitemap.get-articles-routes ()
  ;;; TOCHECK: is key in article the same as key in storage?
  (maphash #'(lambda (k v)
               (declare (ignore k))
               (sitemap.get-item-route v :changefreq "monthly"))
           *storage-articles*))

(defun sitemap.get-vendors-routes ()
  (let ((res))
    (process-storage
     #'(lambda (group)
         (maphash #'(lambda (key v)
                      (declare (ignore v))
                      (push
                       (list :loc (format nil "http://www.320-8080.ru/~a?vendor=~a"
                                          (hunchentoot:url-encode (key group))
                                          (hunchentoot:url-encode key))
                             :lastmod *sitemap-lastmod-time*
                             :changefreq "daily"
                             :priority "0.5")
                       res))
                  (storage.get-vendors group)))
     'group)
    res))

(defun sitemap.get-static-routes ()
  (mapcar #'(lambda (k)
              (list :loc (format nil "http://www.320-8080.ru/~a" k)
                    :lastmod *sitemap-lastmod-time*
                    :changefreq "monthly"
                    :priority "0.5"))
          (let ((statics))
            (maphash #'(lambda (k v)
                         (if (typep v 'article)
                             (push k statics)))
                     (storage *global-storage*))
            statics)))

(defun sitemap.get-special-routes ()
  (list
   (list :loc (format nil "http://www.320-8080.ru/")
         :lastmod *sitemap-lastmod-time*
         :changefreq "hourly"
         :priority "0.5")
   (list :loc (format nil "http://www.320-8080.ru/articles")
         :lastmod *sitemap-lastmod-time*
         :changefreq "daily"
         :priority "0.5")
   (list :loc (format nil "http://www.320-8080.ru/articles/news")
         :lastmod *sitemap-lastmod-time*
         :changefreq "daily"
         :priority "0.5")
   (list :loc (format nil "http://www.320-8080.ru/articles/papers")
         :lastmod *sitemap-lastmod-time*
         :changefreq "daily"
         :priority "0.5")
   (list :loc (format nil "http://www.320-8080.ru/articles/reviews")
         :lastmod *sitemap-lastmod-time*
         :changefreq "daily"
         :priority "0.5")))

(defun sitemap.get-all-routes-list ()
  (append
   (sitemap.get-groups-routes)
   (sitemap.get-products-routes)
   (sitemap.get-filters-routes)
   (sitemap.get-articles-routes)
   (sitemap.get-vendors-routes)
   (sitemap.get-static-routes)
   (sitemap.get-special-routes)))


(defun sitemap.create-sitemap-file ()
  (log5:log-for info "Create Sitemap.XML: ")
  (setq *sitemap-lastmod-time* (time.get-lastmod-time))
  (let ((filepath (config.get-option "CRITICAL" "path-to-sitemap"))
        (routes (sitemap.get-all-routes-list))
        (number 0))
    (log5:log-for info "Routes number: ~A" (length routes))
    (loop
       :while routes
       :do
       (incf number)
       (with-open-file
           (stream (merge-pathnames (format nil "sitemap~D.xml" number) filepath)
                   :direction :output :if-exists :supersede)
         (format stream "~A" (soy.sitemap:head))
         (loop
            :for i
            :from 0 :to (min (- (length routes) 1) 45000)
            :do
            (format stream "~A~%"
                    (soy.sitemap:route (nth i routes))))
         (format stream "~A" (soy.sitemap:tail)))
       (setf routes (nthcdr 45000 routes)))
    (with-open-file
        (stream (pathname (merge-pathnames "sitemap-index.xml" filepath))
                :direction :output :if-exists :supersede)
      (format stream "~A"
              (soy.sitemap:sitemap-index-file
               (list :names (loop
                               :for i :from 1 :to number
                               :collect (format nil "sitemap~D.xml" i))
                     :lastmod *sitemap-lastmod-time*))))))




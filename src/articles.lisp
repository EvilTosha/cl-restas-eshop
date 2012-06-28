;;;; articles.lisp

(in-package #:eshop)

(defvar *storage-articles* (make-hash-table :test #'equal))

(defclass article ()
  ((key         :initarg :key        :initform nil                             :accessor key)
   (name        :initarg :name       :initform nil                             :accessor name)
   (descr       :initarg :descr      :initform nil                             :accessor descr)
   (bredcrumbs  :initarg :bredcrumbs :initform nil                             :accessor bredcrumbs)
   (rightblock  :initarg :rightblock :initform nil                             :accessor rightblock)
   (title       :initarg :title      :initform nil                             :accessor title)
   (body        :initarg :body       :initform nil                             :accessor body)
   (date        :initarg :date       :initform nil                             :accessor date)
   (header      :initarg :header     :initform nil                             :accessor header)
   (pic         :initarg :pic        :initform nil                             :accessor pic)
   (ctype       :initarg :ctype      :initform "article"                       :accessor ctype) ;; article / static / landscape
   (tags        :initarg :tags       :initform (make-hash-table :test #'equal) :accessor tags)))

;;тэги через запятую
(defun make-tags-table (tags input-string)
  (let ((words (split-sequence:split-sequence #\, input-string)))
    (mapcar #'(lambda (w)
                (let ((pure-tag (string-trim '(#\Space #\Tab #\Newline) w)))
                  (when (servo.valid-string-p pure-tag)
                    (setf (gethash (string-downcase pure-tag) tags) pure-tag))))
            words)))

(defmethod unserialize (filepath (dummy article))
	(let* ((file-content (alexandria:read-file-into-string filepath))
				 (raw (decode-json-from-string file-content))
				 (key (pathname-name filepath))
				 (body (cdr (assoc :body raw)))
				 (breadcrumbs (cdr (assoc :breadcrumbs raw)))
				 (rightblock (cdr (assoc :rightblock raw)))
				 (name (cdr (assoc :name raw)))
				 (date (time.article-decode-date (cdr (assoc :date raw))))
				 (descr (cdr (assoc :descr raw)))
				 (tags-line (cdr (assoc :tags raw)))
				 (title (cdr (assoc :title raw)))
				 (ctype (cdr (assoc :ctype raw)))
         (header (cdr (assoc :header raw)))
         (pic (cdr (assoc :pic raw)))
				 (new (make-instance 'article
														 :key key
														 :name name
														 :descr descr
														 :bredcrumbs breadcrumbs
														 :body body
														 :rightblock rightblock
														 :title title
														 :ctype (if ctype
																				ctype
																				(ctype dummy))
                             :header header
                             :pic pic
														 :date date)))
		(make-tags-table (tags new) tags-line)
		(setf (gethash key *storage-articles*) new)
		;; Возвращаем key статьи
		key))

(defun process-articles-dir (path &optional (ctype "article"))
  "Unserialize articles from directory"
  (loop
     :for file :in (directory (merge-pathnames "*" path))
     :unless (cl-fad:directory-pathname-p file)
     :do
     (log5:log-for info-console "Load article: ~a" file)
     (unserialize file (make-instance 'article :ctype ctype))))


(defun articles.restore ()
  (let ((t-storage))
    (log5:log-for info "Start load articles...")
    (sb-ext:gc :full t)
    (let ((*storage-articles* (make-hash-table :test #'equal)))
      (process-articles-dir (config.get-option "PATHS" "path-to-articles") "article")
      (setf t-storage *storage-articles*))
    (setf *storage-articles* t-storage)
    (sb-ext:gc :full t)
    (log5:log-for info "Finish load articles")))

;;шаблоны
(defun articles.update ()
  (servo.compile-soy "index.soy"
                     "articles.soy"
                     "footer.soy"))


(defun articles.sort (unsort-articles)
  (sort unsort-articles #'> :key #'date))


;;;;;;;;;;;;;;;;;;;;; RENDER ;;;;;;;;;;;;;;;;;;;;;;;;;

(defun get-articles-list (&optional request-get-plist)
  (let ((showall (getf request-get-plist :showall))
        (date (getf request-get-plist :showall)))
    (declare (ignore date))
    (loop
       :for art :being :the hash-values :in *storage-articles*
       :when (or showall (plusp (date art)))
       :collect art)))

(defun get-articles-by-tags (articles-list &optional tags)
  (if (not (servo.valid-string-p tags :unwanted-chars (list #\' #\" #\\ #\~ #\Newline)))
      articles-list
      ;; else
      (let ((tags (split-sequence:split-sequence #\, tags)))
        (loop
           :for art :in articles-list
           :when (every #'(lambda (tag)
                            (gethash (string-downcase tag) (tags art)))
                        tags)
           :collect art))))


(defun articles-view-articles (articles)
  (mapcar #'(lambda (v)
              (list  :name (name v)
                     :date (time.article-encode-date v)
                     :key (key v)
                     :pic (pic v)
                     :descr (descr v)))
          articles))

;; отображение списка статей
(defun articles-page (request-get-plist)
  (let* ((tags (getf request-get-plist :tags))
         (breadcrumbs)
         (menu  (soy.articles:articles-menu (list :tag tags))))
    (if tags
        (setf breadcrumbs
              (format nil "       <a href=\"/\">Главная</a> /
                                  <a href=\"/articles\">Материалы</a> /
                                  ~a" tags))
        ;;else
        (setf breadcrumbs "       <a href=\"/\">Главная</a> /
                                  Материалы"))
    (multiple-value-bind (paginated pager)
        (paginator request-get-plist
                   (articles.sort
                    (get-articles-by-tags
                     (get-articles-list request-get-plist) tags))
                   10)
      (default-page
          (soy.static:main
           (list :menu (render.menu)
                 :breadcrumbs breadcrumbs
                 :subcontent  (soy.articles:articles-main
                               (list :menu menu
                                     :articles (soy.articles:articles-list
                                                (list :pager pager
                                                      :articles
                                                      (mapcar #'(lambda (v)
                                                                  (list :name (name v)
                                                                        :date (time.article-encode-date v)
                                                                        :descr (descr v)
                                                                        :key (key v)
                                                                        :pic (pic v)
                                                                        :tags
                                                                        (if (< 0 (hash-table-count (tags v)))
                                                                            (soy.articles:articles-tags
                                                                             (list :tags
                                                                                   (loop
                                                                                      :for key being the hash-keys using (hash-value name)
                                                                                      :of (tags v)
                                                                                      :collect name)))
                                                                            "")))
                                                              paginated)))))
                 :rightblock (soy.articles:r_b_articles
                              (list :articles (let ((arts (articles.sort (get-articles-by-tags (get-articles-list) "новости"))))
                                                (articles-view-articles (list-filters.limit-end arts 5)))
                                    :articles_1 (let ((arts (articles.sort (get-articles-by-tags (get-articles-list) "Акции"))))
                                                  (articles-view-articles (list-filters.limit-end arts 5)))
                                    :articles_2 (let ((arts (articles.sort (get-articles-by-tags (get-articles-list) "обзоры"))))
                                                  (articles-view-articles (list-filters.limit-end arts 5)))))))))))

(defun get-article-breadcrumbs (article)
  (format nil "<a href=\"/\">Главная</a> /
               <a href=\"/articles\">Материалы</a> /
               ~a " (name article)))

(defmethod articles.show-static ((object article))
	(soy.index:main (list :keywords "" ;;keywords
                        :description "" ;;description
                        :title (name object)
                        :header (soy.header:header (append (list :cart (soy.index:cart)
                                                                 :bannertype "line-text")
                                                           (main-page-show-banner "line-text" (banner *main-page.storage*))))
                        :footer (soy.footer:footer)
                        :content  (soy.static:main
                                   (list :menu (render.menu)
                                         :breadcrumbs (bredcrumbs object)
                                         :subcontent  (body object)
                                         :rightblock  (rightblock object))))))

(defmethod articles.show-article  ((object article))
	(soy.index:main (list :keywords "" ;;keywords
                        :description "" ;;description
                        :title  (if (title object)
                                    (title object)
                                    (name object))
                        :headext (soy.articles:head-share-buttons (list :key (key object)))
                        :header (soy.header:header (append (list :cart (soy.index:cart))
                                                           (main-page-show-banner "line" (banner *main-page.storage*))))
                        :footer (soy.footer:footer)
                        :content (soy.static:main
                                  (list :menu (render.menu)
                                        :breadcrumbs (get-article-breadcrumbs object)
                                        :subcontent  (soy.articles:article-big (list :sharebuttons (soy.articles:share-buttons
                                                                                                    (list :key (key object)))
                                                                                     :name (name object)
                                                                                     :date (unless (zerop (date object))
                                                                                             (time.article-encode-date object))
                                                                                     :body (prerender-string-replace (body object))
                                                                                     :articles ""
                                                                                     :tags
                                                                                     (if (plusp (hash-table-count (tags object)))
                                                                                         (soy.articles:articles-tags
                                                                                          (list :tags
                                                                                                (loop
                                                                                                   :for key being the hash-keys using (hash-value name)
                                                                                                   :of (tags object)
                                                                                                   :collect name)))
                                                                                         "")))
                                        :rightblock (soy.articles:r_b_articles
                                                     (list :articles (let ((arts (articles.sort
                                                                                  (remove-if #'(lambda(v)(equal v object))
                                                                                             (get-articles-by-tags (get-articles-list) "новости")))))
                                                                       (articles-view-articles (list-filters.limit-end arts 5)))
                                                           :articles_1 (let ((arts (articles.sort
                                                                                    (remove-if #'(lambda(v)(equal v object))
                                                                                               (get-articles-by-tags (get-articles-list) "Акции")))))
                                                                         (articles-view-articles (list-filters.limit-end arts 5)))
                                                           :articles_2 (let ((arts (articles.sort
                                                                                    (remove-if #'(lambda(v)(equal v object))
                                                                                               (get-articles-by-tags (get-articles-list) "обзоры")))))
                                                                         (articles-view-articles (list-filters.limit-end arts 5))))))))))

(defmethod articles.show-landscape  ((object article))
	(soy.index:main-landscape (list :keywords "" ;;keywords
                                  :description "" ;;description
                                  :headeraddition (header object)
                                  :title (name object)
                                  :content  (body object))))

;; отображение страницы статьи
(defmethod restas:render-object ((designer eshop-render) (object article))
  (cond ((equal (ctype object) "static") (articles.show-static object))
        ((equal (ctype object) "article") (articles.show-article object))
        ((equal (ctype object) "landscape") (articles.show-landscape object))))

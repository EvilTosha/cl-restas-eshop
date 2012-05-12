;;;; articles.lisp

(in-package #:eshop)

;; хранилище статей
(defvar *storage-articles* (make-hash-table :test #'equal))

;; описание полей статьи
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
   (ctype       :initarg :ctype      :initform "article"                       :accessor ctype) ;; article / static / landscape
   (tags        :initarg :tags       :initform (make-hash-table :test #'equal) :accessor tags)))

;;тэги через запятую
(defun make-tags-table (tags input-string)
  (let ((words (split-sequence:split-sequence #\, input-string)))
    (mapcar #'(lambda (w)
                (when (servo.is-valid-string w)
                  (setf (gethash (stripper w) tags) w)))
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
														 :date date)))
		(make-tags-table (tags new) tags-line)
		(setf (gethash key *storage-articles*) new)
		;; Возвращаем key статьи
		key))

;; загрузка статей из папки
(defun process-articles-dir (path &optional (ctype "article"))
  (let ((files))
    (mapcar #'(lambda (x)
                (unless (cl-fad:directory-pathname-p x)
                  (push x files)))
            (directory (format nil "~a/*" path)))
    (mapcar #'(lambda (file)
                (log5:log-for info-console "Load article: ~a" file)
                (unserialize (format nil "~a" file) (make-instance 'article :ctype ctype)))
            files)))


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
  (let ((articles)
        (showall (getf request-get-plist :showall))
        (date (getf request-get-plist :showall)))
    (declare (ignore date))
    (maphash #'(lambda (k v)
                 (declare (ignore k))
                 (when (or showall
                           (not (zerop (date v))))
                   (push v articles)))
             *storage-articles*)
    articles))


(defun get-articles-by-tags (articles-list &optional tags)
  (let ((articles))
    (if (not (servo.is-valid-string tags :unvanted-chars (list #\' #\" #\\ #\~ #\Newline)))
        (mapcar #'(lambda (v) (push v articles))
                articles-list)
        ;;else
        (progn
          (let ((tags (split-sequence:split-sequence #\, tags)))
            (mapcar #'(lambda (v)
                        (when (every #'(lambda (tag)
                                         (gethash tag (tags v)))
                                     tags)
                          (push v articles)))
                    articles-list))))
    articles))

(defun articles-view-articles (articles)
  (mapcar #'(lambda (v)
              (list  :name (name v)
                     :date (time.article-encode-date v)
                     :key (key v)))
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
           (list :menu (new-classes.menu)
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
                                                                        :tags
                                                                        (if (< 0 (hash-table-count (tags v)))
                                                                            (soy.articles:articles-tags
                                                                             (list :tags
                                                                                   (loop
                                                                                      :for key being the hash-keys
                                                                                      :of (tags v)
                                                                                      :collect key)))
                                                                            "")))
                                                              paginated)))))
                 :rightblock (soy.articles:r_b_articles (list :articles (let ((articles (articles.sort (get-articles-list))))
                                                                          (when articles
                                                                            (articles-view-articles (subseq articles 0 10))))))))))))

(defun get-article-breadcrumbs (article)
  (format nil "<a href=\"/\">Главная</a> /
               <a href=\"/articles\">Материалы</a> /
               ~a " (name article)))

(defmethod articles.show-static ((object article))
	(root:main (list :keywords "" ;;keywords
                   :description "" ;;description
                   :title (name object)
                   :header (root:header (append (list :cart (root:cart))
                                                (main-page-show-banner "line-text" (banner *main-page.storage*))))
                   :footer (soy.footer:footer)
                   :content  (soy.static:main
                              (list :menu (new-classes.menu)
                                    :breadcrumbs (bredcrumbs object)
                                    :subcontent  (body object)
                                    :rightblock  (rightblock object))))))

(defmethod articles.show-article  ((object article))
	(root:main (list :keywords "" ;;keywords
									 :description "" ;;description
									 :title  (if (title object)
															 (title object)
															 (name object))
									 :headext (soy.articles:head-share-buttons (list :key (key object)))
									 :header (root:header (append (list :cart (root:cart))
																								(main-page-show-banner "line" (banner *main-page.storage*))))
									 :footer (soy.footer:footer)
									 :content (soy.static:main
														 (list :menu (new-classes.menu)
																	 :breadcrumbs (get-article-breadcrumbs object)
																	 :subcontent  (soy.articles:article-big (list :sharebuttons (soy.articles:share-buttons
																																															 (list :key (key object)))
																																								:name (name object)
																																								:date (unless (zerop (date object))
                                                                                        (time.article-encode-date object))
																																								:body (prerender-string-replace (body object))
																																								:articles (let ((articles (articles.sort (remove-if #'(lambda(v)(equal v object)) (get-articles-list)))))
																																														(when articles
																																																(articles-view-articles (subseq articles 0 7))))
																																								:tags
																																								(if (minusp (hash-table-count (tags object)))
																																										(soy.articles:articles-tags
																																										 (list :tags
																																													 (loop
																																															:for key being the hash-keys
																																															:of (tags object)
																																															:collect key)))
																																										"")))
																	 :rightblock (soy.articles:r_b_articles (list :articles (let ((articles (articles.sort (remove-if #'(lambda(v)(equal v object)) (get-articles-list)))))
																																														(when articles
                                                                                              (articles-view-articles (subseq articles 0 10)))))))))))

(defmethod articles.show-landscape  ((object article))
	(root:main-landscape (list :keywords "" ;;keywords
                             :description "" ;;description
                             :headeraddition (header object)
                             :title (name object)
                             :content  (body object))))

;; отображение страницы статьи
(defmethod restas:render-object ((designer eshop-render) (object article))
  (cond ((equal (ctype object) "static") (articles.show-static object))
        ((equal (ctype object) "article") (articles.show-article object))
        ((equal (ctype object) "landscape") (articles.show-landscape object))))

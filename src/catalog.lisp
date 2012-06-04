;;;; catalog.lisp

(in-package #:eshop)

;;шаблоны
(defun catalog.catalog-update ()
  (servo.compile-soy "new-catalog.soy"))

(defun catalog.sitemap-entity (error404  &optional col1 col2)
  (soy.new-catalog:catalog-main
   (list
    :error404 error404
    :numproducts (length (active-products *global-storage*))
    :menu (class-core.menu)
    :items (let ((res)
                 (roots (root-groups *global-storage*))
                 (exception "bytovaya-technika"))
             (setf roots (remove-if #'(lambda (root)
                                        (string= (key root) exception))
                                    roots))
             (setf roots (append roots
                                 (copy-list (groups (gethash exception (storage *global-storage*))))))
             (setf roots (sort roots #'class-core.menu-sort))
             (setf res
                   (mapcar #'(lambda (node)
                               (format nil "~a"
                                       (soy.new-catalog:catalog-item
                                        (let* ((pic (unless (equal "" (pic node))
                                                      (pic node)))
                                               (style (when pic
                                                        (multiple-value-bind (width height)
                                                            (images-get-dimensions
                                                             (format nil "~a/htimgs~a" *path-to-dropbox* (pic node)))
                                                          (images-style-for-resize width height 70)))))
                                          (list
                                           :other nil
                                           :maingrouplink (format nil "<a href=\"/~a\">~a</a>~%" (key node) (name node))
                                           :maingroupimg pic
                                           :imgstyle style
                                           :groups (mapcar #'(lambda (g)
                                                               (format nil "<a href=\"/~a\">~a</a>~%" (key g) (name g)))
                                                           (storage.get-all-child-groups node)))))))
                           roots))
             (when col1
               (nconc res (list col1)))
             (when col2
               (nconc res (list col2)))
             res))))


(defun catalog.catalog-entity ()
  (catalog.sitemap-entity nil))

(defun catalog.sitemap-page (&optional error404)
  (catalog.sitemap-entity
   error404
   (soy.new-catalog:catalog-item
    (list
     :maingrouplink "Покупателям"
     :maingroupimg nil
     :other t))))

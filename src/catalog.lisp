;;;; catalog.lisp

(in-package #:eshop)

;;шаблоны
(defun catalog.catalog-update ()
  (servo.compile-soy "new-catalog.soy"))

(defun catalog.sitemap-entity (error404  &optional col1 col2)
  (soy.new-catalog:catalog-main
   (list
    :error404 error404
    :numproducts (count-storage 'product)
    :menu (render.menu)
    :items (let ((res)
                 (roots (get-root-groups))
                 (exception "bytovaya-technika"))
             (setf roots (remove exception roots :key #'key :test #'equal))
             (setf roots (append roots
                                 (copy-list (groups (getobj exception 'group)))))
             (setf res
                   (mapcar #'(lambda (node)
                               (format nil "~A"
                                       (soy.new-catalog:catalog-item
                                        (let* ((pic (unless (equal "" (pic node))
                                                      (pic node)))
                                               (style (when pic
                                                        (let ((dimensions
                                                               (get-dimensions
                                                                (format nil "~a/htimgs~a" *path-to-dropbox* (pic node)))))
                                                          (style-for-resize (getf dimensions :width)
                                                                            (getf dimensions :height)
                                                                            70)))))
                                          (list
                                           :other nil
                                           :maingroupname (name node)
                                           :maingroupurl (key node)
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
     :other t
     :maingroupname "Покупателям"
     :maingroupurl nil
     :maingroupimg nil
     :other t))))

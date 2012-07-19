;;;; classes.lisp

(in-package #:eshop)

(class-core.make-class-and-methods
 product
 ((:name key               :initform ""                     :disabled t     :type string      :serialize t)
  (:name articul           :initform nil                    :disabled t     :type int         :serialize t)
  (:name name-provider     :initform ""                     :disabled nil   :type string      :serialize t)
  (:name name-seo          :initform ""                     :disabled nil   :type string      :serialize t)
  (:name siteprice         :initform 0                      :disabled nil   :type int         :serialize t)
  (:name delta-price       :initform 0                      :disabled nil   :type int         :serialize t)
  (:name bonuscount        :initform 0                      :disabled nil   :type int         :serialize t)
  (:name delivery-price    :initform nil                    :disabled nil   :type int         :serialize t)
  (:name active            :initform t                      :disabled nil   :type bool        :serialize nil)
  (:name preorder          :initform nil                    :disabled nil   :type bool        :serialize t)
  (:name newbie            :initform t                      :disabled nil   :type bool        :serialize t)
  (:name sale              :initform t                      :disabled nil   :type bool        :serialize t)
  (:name parents           :initform nil                    :disabled nil   :type group-list  :serialize t)
  (:name date-modified     :initform (get-universal-time)   :disabled t     :type time        :serialize t)
  (:name date-created      :initform (get-universal-time)   :disabled t     :type time        :serialize t)
  (:name seo-text          :initform ""                     :disabled nil   :type textedit    :serialize t)
  (:name count-transit     :initform 0                      :disabled t     :type int         :serialize t)
  (:name count-total       :initform 0                      :disabled t     :type int         :serialize t)
  (:name optgroups         :initform nil                    :disabled t     :type optgroups   :serialize nil)
  (:name vendor            :initform ""                     :disabled nil   :type string      :serialize t))
 :storage-size 70000)


(class-core.make-class-and-methods
 group
 ((:name key                 :initform nil                             :disabled t     :type string                    :serialize t)
  (:name yml-id              :initform (yml.get-next-yml-id)           :disabled t     :type int                       :serialize t)
  (:name parents             :initform nil                             :disabled nil   :type group-list                :serialize t)
  (:name name                :initform nil                             :disabled nil   :type string                    :serialize t)
  (:name active              :initform nil                             :disabled nil   :type bool                      :serialize t)
  (:name empty               :initform nil                             :disabled t     :type bool                      :serialize nil)
  (:name order               :initform 1000                            :disabled nil   :type int                       :serialize t)
  (:name ymlshow             :initform nil                             :disabled nil   :type bool                      :serialize t)
  (:name pic                 :initform nil                             :disabled nil   :type string                    :serialize t)
  (:name icon                :initform nil                             :disabled nil   :type string                    :serialize t)
  (:name delivery-price      :initform nil                             :disabled nil   :type int                       :serialize t)
  (:name groups              :initform nil                             :disabled t     :type group-list                :serialize nil)
  (:name products            :initform nil                             :disabled t     :type product-list              :serialize nil)
  (:name filters             :initform nil                             :disabled t     :type undefined                 :serialize nil)
  (:name fullfilter          :initform nil                             :disabled t     :type undefined                 :serialize nil)
  (:name raw-fullfilter      :initform nil                             :disabled nil   :type textedit-raw              :serialize t)
  (:name vendors             :initform (make-hash-table :test #'equal) :disabled t     :type undefined #||hash-table||#:serialize nil)
  (:name seo-text            :initform nil                             :disabled nil   :type textedit                  :serialize t)
  (:name upsale-links        :initform nil                             :disabled nil   :type group-list                :serialize t)
  (:name keyoptions          :initform nil                             :disabled nil   :type keyoptions                :serialize t)
  (:name catalog-keyoptions  :initform nil                             :disabled nil   :type catalog-keyoptions        :serialize t)
  (:name life-time           :initform 100                             :disabled nil   :type int                       :serialize t))
 :storage-size 400)


(class-core.make-class-and-methods
 filter
 ((:name key               :initform ""       :disabled t    :type string)
  (:name parents           :initform nil      :disabled t    :type group-list)
  (:name name              :initform ""       :disabled nil  :type string)
  (:name func              :initform ""       :disabled t    :type string)
  (:name func-string       :initform ""       :disabled t    :type textedit))
 :serialize nil)

(class-core.make-class-and-methods
 vendor
 ((:name key       :initform ""                              :disabled t   :type string             :serialize t)
  (:name name      :initform ""                              :disabled nil :type string             :serialize t)
  (:name alias     :initform ""                              :disabled nil :type string             :serialize t)
  (:name seo-texts :initform (make-hash-table :test #'equal) :disabled t   :type textedit-hashtable :serialize t))
 :storage-size 300)

(defclass group-filter ()
  ((name              :initarg :name            :initform nil       :accessor name)
   (base              :initarg :base            :initform nil       :accessor base)
   (advanced          :initarg :advanced        :initform nil       :accessor advanced)))



;; TODO: rewrite options store mechanism (with using string literal -> id convertion)
(defmacro with-option1 (product optgroup-name option-name body)
  `(mapcar #'(lambda (optgroup)
               (when (string= (getf optgroup :name) ,optgroup-name)
                 (let ((options (getf optgroup :options)))
                   (mapcar #'(lambda (option)
                               (if (string= (getf option :name) ,option-name)
                                   ,body))
                           options))))
           (optgroups ,product)))

;; Beware of names! (dumb-written macro with-option1)
(defun get-option (product opgroup optname)
  (declare (product product) (string opgroup optname))
  (let (res)
    (with-option1 product
      opgroup optname
      (setf res (getf option :value)))
    res))

;; TODO: handle case when no option found
(defun set-option (product opgroup optname value)
  (declare (product product) (string opgroup optname))
  (with-option1 product
    opgroup optname
    (setf (getf option :value) value)))

(defmethod price ((product product))
  (+ (siteprice product) (delta-price product)))

(defun classes.has-vendor-seo-text (group vendor-key)
  "Chech whether there is vendor's seo-text for given group"
  (and group (servo.valid-string-p vendor-key)
       (let ((vendor-obj (gethash (string-downcase vendor-key) (vendors group))))
         (and vendor-obj (gethash (key group) (seo-texts vendor-obj))))
       t))

(defun classes.get-group-seo-text (group &optional vendor-key)
  "If vendor passed, try to return corresponding seo-text for group,
if there is not one, or no vendor passed, return group's seo-text"
  (declare (group group))
  (let ((vendor-object (when (servo.valid-string-p vendor-key)
                         (gethash (string-downcase vendor-key) (vendors group)))))
    (aif (and vendor-object (gethash (key group) (seo-texts vendor-object)))
         it             ; if condition non-nil, it is required seo-text
         ;; else
         (seo-text group))))

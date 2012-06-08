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
  (:name vendor            :initform ""                     :disabled nil   :type string      :serialize t)))


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
  (:name filters             :initform nil                             :disabled t     :type string                    :serialize nil)
  (:name fullfilter          :initform nil                             :disabled t     :type string                    :serialize nil)
  (:name raw-fullfilter      :initform nil                             :disabled nil   :type textedit-raw              :serialize t)
  (:name vendors             :initform (make-hash-table :test #'equal) :disabled t     :type textedit-hashtable        :serialize nil)
  (:name seo-text            :initform nil                             :disabled nil   :type textedit                  :serialize t)
  (:name upsale-links        :initform nil                             :disabled nil   :type group-list                :serialize t)
  (:name keyoptions          :initform nil                             :disabled nil   :type keyoptions                :serialize t)
  (:name catalog-keyoptions  :initform nil                             :disabled nil   :type catalog-keyoptions        :serialize t)
  (:name life-time           :initform 100                             :disabled nil   :type int                       :serialize t)))


(class-core.make-class-and-methods
 filter
 ((:name key               :initform ""       :disabled t    :type string)
  (:name parents           :initform nil      :disabled t    :type group-list)
  (:name name              :initform ""       :disabled nil  :type string)
  (:name func              :initform ""       :disabled t    :type string)
  (:name func-string       :initform ""       :disabled t    :type textedit)))

(class-core.make-class-and-methods
 vendor
 ((:name key       :initform ""                              :disabled t   :type string             :serialize t)
  (:name name      :initform ""                              :disabled nil :type string             :serialize t)
  (:name alias     :initform ""                              :disabled nil :type string             :serialize t)
  (:name seo-texts :initform (make-hash-table :test #'equal) :disabled t   :type textedit-hashtable :serialize t)))

(defclass group-filter ()
  ((name              :initarg :name            :initform nil       :accessor name)
   (base              :initarg :base            :initform nil       :accessor base)
   (advanced          :initarg :advanced        :initform nil       :accessor advanced)))

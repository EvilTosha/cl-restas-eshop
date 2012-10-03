;;;; slots.lisp

(in-package #:eshop)

(defun slots.string-delete-newlines (string)
  "Замена символов перевода каретки на пробел (актуально при сериализации имен групп)"
  (substitute #\Space #\Newline string))

(defgeneric slots.parents-fix (item)
  (:documentation "Remove given item from children lists, and
add it to such lists for its parents")
  (:method (item) #| do nothing by default |#))

(defmethod slots.parents-fix ((item product))
  "Remove given product from all groups' product lists and
add it to all parents' lists"
  (process-storage #'(lambda (group)
                       (setf (products group)
                             (remove (key item) (products group)
                                     :key #'key :test #'equal)))
                   'group)
  ;; add to parents
  (mapcar #'(lambda (group)
              (pushnew item (products group)))
          (parents item)))

(defmethod slots.parents-fix ((item group))
  "Remove given group from all groups' children lists and
add it to all parents' lists"
  (process-storage #'(lambda (group)
                       (setf (groups group)
                             (remove (key item) (groups group)
                                     :key #'key :test #'equal)))
                   'group)
  ;;add to parents
  (mapcar #'(lambda (group)
              (pushnew item (groups group)))
          (parents item)))

(defmethod slots.parents-fix ((item filter))
  "Remove given filter from all groups' filter lists and
add it to all parents' lists"
  (process-storage #'(lambda (group)
                       (remhash (key item) (filters group)))
                   'group)
  ;;add to parents
  (mapcar #'(lambda (group)
              (setf (gethash (key item) (filters group)) item))
          (parents item)))


;; набор функций для показа, изменения и сериализации различных типов полей в админке
;; тип поля указывается в :type при создании класса (см. class-core.lisp)

(defgeneric slots.%view (type value name disabled)
  (:documentation "Viewing slot in admin-table"))

(defgeneric slots.%get-data (type post-data-string)
  (:documentation "Decoding from post data in admin table"))

(defgeneric slots.%encode-to-string (type value)
  (:documentation "Converting slot value to string (not json string).
 Usually used for serializing"))

(defgeneric slots.%decode-from-string (type string)
  (:documentation "Decoding slot value from string (not json string).
Usually used for unserializing from backup entry"))

(defgeneric slots.%serialize-p (type value &optional initform)
  (:documentation "Check whether slot-value needs serialization. Usually it checks
for not being equal to initform.
Note: initform is passed unevaluated"))

;;; Standard error/default methods, used when no more specific method defined
(defmethod slots.%get-data (type post-data-string)
  (declare (string post-data-string))
  (error "Attempt to get data for slot that couldn't be represented in admin-table"))

(defmethod slots.%encode-to-string (type value)
  (declare (ignore value))
  (error "Attempt to encode to string value of type, that hasn't string represenatation.
Type: ~A" type))

(defmethod slots.%decode-from-string (type string)
  (declare (ignore string))
  (error "Attempt to decode from string slot, that hasn't string reperesentation.
Type: ~A" type))

(defmethod slots.%serialize-p (type value &optional initform)
  (and value
       (valid-string-p (format nil "~A" value))
       (not (equal value (eval initform)))))

;;string - строковый тип
(defmethod slots.%view ((type (eql 'string)) value name disabled)
  (soy.class_forms:string-field
   (list :name name :disabled disabled :value value)))

(defmethod slots.%get-data ((type (eql 'string)) post-data-string)
  (declare (string post-data-string))
  (decode-json-from-string post-data-string))

(defmethod slots.%encode-to-string ((type (eql 'string)) value)
  value)

(defmethod slots.%decode-from-string ((type (eql 'string)) string)
  (declare (string string))
  string)


;; undefined - used for types that couldn't be edited from admin-table and
;; dont have string representation
(defmethod slots.%view ((type (eql 'undefined)) value name disabled)
  ;; always disabled
  (if disabled
      (slots.%view 'string value name t)
      ;; else
      (error "Slot of type undefined, but isn't disabled. Name: ~A" name)))

;;int
(defmethod slots.%view ((type (eql 'int)) value name disabled)
  (soy.class_forms:integer-field
   (list :name name :disabled disabled :value value)))

(defmethod slots.%get-data ((type (eql 'int)) post-data-string)
  (declare (string post-data-string))
  (let ((int (decode-json-from-string post-data-string)))
    (if (integerp int)
        int
        (error "Error: value ~A is not integer" int))))

(defmethod slots.%encode-to-string ((type (eql 'int)) value)
  (format nil "~D" value))

(defmethod slots.%decode-from-string ((type (eql 'int)) string)
  (declare (string string))
  (parse-integer string))

;; string-plist, property list of strings
;; Note: all digits in keys should be separated by hyphens:
;; (list :foo1 "bar" :foo2bar "baz") - Bad
;; (list :foo-1 "bar" :foo-2-bar "baz") - Good
(defmethod slots.%view ((type (eql 'string-plist)) value name disabled)
  (soy.class_forms:data-field
   (list :pairs (let (pairs)
                  (sb-pcl::doplist (key val) value
                    (push (list :name (format nil "~A" key)
                                :value val) pairs))
                  pairs)
         :name name
         :disabled disabled)))

(defmethod slots.%get-data ((type (eql 'string-plist)) post-data-string)
  (declare (string post-data-string))
  (loop :for pair :in (decode-json-from-string post-data-string)
     :for pair-plist := (servo.alist-to-plist pair)
     :collect (anything-to-keyword (getf pair-plist :name))
     :collect (getf pair-plist :value)))

(defmethod slots.%encode-to-string ((type (eql 'string-plist)) value)
  (encode-json-plist-to-string value))

(defmethod slots.%decode-from-string ((type (eql 'string-plist)) string)
  (declare (string string))
  (servo.alist-to-plist
   (decode-json-from-string string)))

;; symbol, standard symbol
(defmethod slots.%view ((type (eql 'symbol)) value name disabled)
  (slots.%view 'string (string value) name disabled))

(defmethod slots.%get-value ((type (eql 'symbol)) post-data-string)
  (declare (string post-data-string))
  (anything-to-symbol (slots.%get-data 'string post-data-string)))

(defmethod slots.%encode-to-string ((type (eql 'symbol)) value)
  ;; doesn't work with symbols starting with digits
  (declare (symbol value))
  (string value))

(defmethod slots.%decode-from-string ((type (eql 'symbol)) string)
  (declare (string string))
  (anything-to-symbol string))

(defmethod slots.%serialize-p ((type (eql 'symbol)) value &optional initform)
  ;; symbol always should be serializable
  (declare (ignore initform))
  t)

;; declare type specifier for convenient type checks
(deftype default-set ()
  `(or symbol list filter))

;; default-set, значение выборки для фильра по умолчанию
(defmethod slots.%view ((type (eql 'default-set)) value name disabled)
  (declare (default-set value) (string name) (boolean disabled))
  ;; TODO: write proper viewer
  (slots.%view 'string value name disabled))

(defmethod slots.%get-data ((type (eql 'default-set)) post-data-string)
  (declare (string post-data-string))
  ;; TODO: write proper get-data
  post-data-string)

(defmethod slots.%encode-to-string ((type (eql 'default-set)) value)
  (declare (default-set value))
  (encode-json-to-string
   (etypecase value
     (symbol `(symbol ,value))
     ;; Note: each element of list must have method "key"
     ;; and can be found by "getobj" method
     (list `(list ,(mapcar #'key value)))
     (filter `(filter ,(key value))))))

(defmethod slots.%decode-from-string ((type (eql 'default-set)) string)
  (declare (string string))
  (let ((decoded-list (decode-json-from-string string)))
    (string-case (first decoded-list)
      ("symbol" (anything-to-symbol (second decoded-list)))
      ("list" (keys-to-objects (second decoded-list)))
      ("filter" (getobj (second decoded-list) 'filter)))))


(defmethod slots.%view ((type (eql 'filters-hash-table)) value name disabled)
  (declare (hash-table value) (string name) (boolean disabled))
  (labels ((get-variants (data prefix)
             (loop
                :for i :from 0
                :for variant := (format nil "~A~D" prefix i)
                :for variant-value := (getf data (anything-to-keyword variant))
                :while variant-value
                :collect (list :name variant
                               :value variant-value
                               ;; empty for now rewrite later
                               :placeholder (format nil "Variant ~A" i))))
           (prepare-fields (filter-type data)
             (let ((filter-data (gethash (anything-to-symbol filter-type)
                                         *basic-filters-data*)))
               (loop :for key :being :the hash-keys :in (fields filter-data)
                  :using (hash-value field-params)
                  :collect (if (equal (getf field-params :type) 'multi)
                               (list :type "multi" :label (getf field-params :name)
                                     :variants (get-variants data (getf field-params :prefix)))
                               ;; else
                               (list :type "single" :label (getf field-params :name)
                                     :name (format nil "~(~A~)" key)
                                     :value (getf data (anything-to-keyword key))))))))
    (soy.class_forms:filter-hash-table-field
     (list :disabled disabled
           :name name
           :types (filters.get-basics-types)
           :filters (loop :for filter :being :the hash-values :in value
                       :collect (list :types (filters.get-basics-types
                                              (filter-type filter))
                                      :fields (prepare-fields (filter-type filter)
                                                              (data filter))))))))

(defmethod slots.%get-data ((type (eql 'filters-hash-table)) post-data-string)
  (declare (string post-data-string))
  (let ((basics (servo.recursive-alist-to-plist (decode-json-from-string post-data-string)))
        (result (make-hash-table :test #'equal)))
    (sb-pcl::doplist
        (key filter) basics
        (setf (gethash key result)
              (apply #'filters.create-basic-filter
                     (anything-to-symbol (getf filter :type))
                     (getf filter :data))))
    result))


(defmethod slots.%encode-to-string ((type (eql 'filters-hash-table)) hash-table)
  (declare (hash-table hash-table))
  (encode-json-to-string
   (loop
      :for key :being :the hash-keys :in hash-table
      :using (hash-value filter)
      :collect (cons
                key
                (encode-json-to-string
                 (backup.serialize-entity filter))))))

(defmethod slots.%decode-from-string ((type (eql 'filters-hash-table)) string)
  (declare (string string))
  (let ((decoded-list (decode-json-from-string string))
        (hash-table (make-hash-table :test #'equal)))
    (loop
       :for (key . value) :in decoded-list
       :do (let ((decoded-list (decode-json-from-string value)))
             (setf (gethash (anything-to-symbol key) hash-table)
                   (%unserialize 'basic-filter decoded-list))))
    hash-table))


;;textedit, онлайновый WYSIWYG редактор текста
(defmethod slots.%view ((type (eql 'textedit)) value name disabled)
  (if disabled
      (slots.%view 'string value name disabled)
      (soy.class_forms:texteditor
       (list :name name :value value))))

(defmethod slots.%get-data ((type (eql 'textedit)) post-data-string)
  "Replace #\Replace (#\Newline remains)"
  (servo.string-replace-chars (decode-json-from-string post-data-string) (list #\Return)))

(defmethod slots.%encode-to-string ((type (eql 'textedit)) value)
  (slots.%encode-to-string 'string value))

(defmethod slots.%decode-from-string ((type (eql 'textedit)) string)
  (declare (string string))
  (slots.%decode-from-string 'string string))

;;textedit-raw, текстовое поле для правки кода
(defmethod slots.%view ((type (eql 'textedit-raw)) value name disabled)
  (if disabled
      (slots.%view 'string value name disabled)
      (soy.class_forms:texteditor-raw
       (list :name name :value value))))

(defmethod slots.%get-data ((type (eql 'textedit-raw)) post-data-string)
  (slots.%get-data 'textedit post-data-string))

(defmethod slots.%encode-to-string ((type (eql 'textedit-raw)) value)
  (slots.%encode-to-string 'textedit value))

(defmethod slots.%decode-from-string ((type (eql 'textedit-raw)) string)
  (declare (string string))
  (slots.%decode-from-string 'textedit string))

;;textedit-hashtable, string-string hashtable
;; TODO: write proper methods
(defmethod slots.%view ((type (eql 'textedit-hashtable)) value name disabled)
  (slots.%view 'undefined value name disabled))

(defmethod slots.%get-data ((type (eql 'textedit-hashtable)) string)
  (declare (string string))
  (slots.%get-data 'undefined string))

(defmethod slots.%encode-to-string ((type (eql 'textedit-hashtable)) hashtable)
  (let ((res-list))
    (maphash #'(lambda (vendor seo-text)
                 (push
                  (format nil "~A,~A"
                          (encode-json-to-string vendor)
                          (encode-json-to-string
                           (slots.%encode-to-string 'textedit seo-text)))
                  res-list))
             hashtable)
    (when res-list
      (format nil "[~{~A~^,~}]" res-list))))

(defmethod slots.%decode-from-string ((type (eql 'textedit-hashtable)) string)
  (declare (string string))
  (servo.list-to-hashtasble (decode-json-from-string string)))

;;time, человекопонятное время
(defmethod slots.%view ((type (eql 'time)) value name disabled)
  (slots.%view 'string (time.decode-date-time value) name disabled))

(defmethod slots.%get-data ((type (eql 'time)) post-data-string)
  ;; TODO: decode (and make view)
  post-data-string)

(defmethod slots.%encode-to-string ((type (eql 'time)) value)
  (slots.%encode-to-string 'int value))

(defmethod slots.%decode-from-string ((type (eql 'time)) string)
  (declare (string string))
  (slots.%decode-from-string 'int string))

;;bool
(defmethod slots.%view ((type (eql 'bool)) value name disabled)
  (declare (boolean value) (string name) (boolean disabled))
  (soy.class_forms:bool-field
   (list :name name :checked value :disabled disabled)))

(defmethod slots.%get-data ((type (eql 'bool)) post-data-string)
  (declare (string post-data-string))
  (let ((bool (decode-json-from-string post-data-string)))
    (if (typep bool 'boolean)
        bool
        (error "Error: value ~A is not bool" bool))))

(defmethod slots.%encode-to-string ((type (eql 'bool)) value)
  (format nil "~A" value))

(defmethod slots.%decode-from-string ((type (eql 'bool)) string)
  (declare (string string))
  (read-from-string string))

;;генерация дерева групп
;;open - список групп до которых нужно раскрыть дерево
;;field-name - название поля, нужно для проставления в name в radio
(defun slots.group-tree (open-groups field-name)
  (soy.class_forms:group-tree
   (list :roots
         (mapcar #'(lambda (child)
                     (slots.group-branch child open-groups field-name))
                 (get-root-groups)))))


;;group, список групп, генерируется из списка с проставленными уровнями глубины
(defmethod slots.%view ((type (eql 'group-list)) value name disabled)
  (soy.class_forms:group-field (list :name name
                                     :tree (slots.group-tree value name)
                                     :disabled disabled)))

;;генерация ветви дерева с корнем в данной группе
;;group - корень ветви, open-groups - список групп, до которых нужно раскрыть дерево
;;field-name - имя поля, нужно для проставления в name в radio
(defun slots.group-branch (group open-groups field-name)
  (let* ((open
          ;;выясняем нужно ли открывать группу
          (some
           #'(lambda (open-group)
               (eq (key group) (key open-group)))
           open-groups))
         (checked open)
         child-open
         children)
    ;;строим список дочерних ветвей и проверяем нужно ли открыть данный узел
    (setf children
          (mapcar #'(lambda (child)
                      (multiple-value-bind (branch branch-opened)
                          (slots.group-branch child open-groups field-name)
                        (setf child-open (or child-open branch-opened))
                        branch))
                  (groups group)))
    (values-list (list
                  (soy.class_forms:group-tree-branch (list :opened child-open
                                                           :hashkey (key group)
                                                           :name (name group)
                                                           :checked checked
                                                           :children children
                                                           :fieldname field-name))
                  (or open child-open)))))


(defmethod slots.%get-data ((type (eql 'group-list)) string-list)
  (when string-list
    (keys-to-objects (decode-json-from-string string-list) :type 'group)))

(defmethod slots.%encode-to-string ((type (eql 'group-list)) groups)
  (format nil "[~{\"~A\"~^,~}]" (mapcar #'key groups)))

(defmethod slots.%decode-from-string ((type (eql 'group-list)) string)
  ;; ! Needs post-processing
  (declare (string string))
  (decode-json-from-string string))

;;products, список продуктов(-детей)
(defmethod slots.%view ((type (eql 'product-list)) value name disabled)
  (slots.%view 'string value name disabled))

(defmethod slots.%get-data ((type (eql 'product-list)) string-list)
  (when string-list
    (keys-to-objects (ensure-list string-list) :type 'product)))

(defmethod slots.%encode-to-string ((type (eql 'product-list)) products)
  (format nil "[~{\"~A\"~^,~}]" (mapcar #'key products)))

(defmethod slots.%decode-from-string ((type (eql 'product-list)) string)
  ;; ! Needs post-processing
  (declare (string string))
  (decode-json-from-string string))

;;optgroups
(defmethod slots.%view ((type (eql 'optgroups)) value name disabled)
  (slots.%view 'string value name disabled))

(defmethod slots.%get-data ((type (eql 'optgroups)) post-data-string)
  (slots.%get-data 'string post-data-string))

;;keyoptions
(defmethod slots.%view ((type (eql 'keyoptions)) value name disabled)
  (soy.class_forms:keyoptions-field
   (list :keyoptions value
         :name name
         :disabled disabled)))

(defmethod slots.%get-data ((type (eql 'keyoptions)) post-data-string)
  (declare (string post-data-string))
  (let ((keyoptions-list (decode-json-from-string post-data-string)))
    (remove-if #'null
               (mapcar
                #'(lambda (keyoption-alist)
                    (let ((keyoption (servo.alist-to-plist keyoption-alist)))
                      (when (and (getf keyoption :optgroup)
                                 (getf keyoption :optname)
                                 (getf keyoption :showname))
                        keyoption)))
                keyoptions-list))))


(defmethod slots.%encode-to-string ((type (eql 'keyoptions)) keyoptions)
  (format nil "[~{~a~^,~}]"
          (mapcar #'(lambda (keyoption)
                      (format nil "{\"optgroup\":~a,\"optname\":~a,\"showname\":~a,\"units\":~a}"
                              (encode-json-to-string (getf keyoption :optgroup))
                              (encode-json-to-string (getf keyoption :optname))
                              (encode-json-to-string (getf keyoption :showname))
                              (encode-json-to-string (or (getf keyoption :units) ""))))
                  keyoptions)))

(defmethod slots.%decode-from-string ((type (eql 'keyoptions)) string)
  (declare (string string))
  (mapcar #'(lambda (pair)
              (list :optgroup (cdr (assoc :optgroup pair))
                    :optname (cdr (assoc :optname pair))
                    :showname (cdr (assoc :showname pair))
                    :units (cdr (assoc :units pair))))
          (decode-json-from-string string)))

(defmethod slots.%view ((type (eql 'catalog-keyoptions)) value name disabled)
  (soy.class_forms:catalog-keyoptions-field
   (list :keyoptions value
         :name name
         :disabled disabled)))

(defmethod slots.%get-data ((type (eql 'catalog-keyoptions)) post-data-string)
  (declare (string post-data-string))
  (let ((keyoptions-list (decode-json-from-string post-data-string)))
    (remove-if #'null
               (mapcar
                #'(lambda (keyoption-alist)
                    (let ((keyoption (servo.alist-to-plist keyoption-alist)))
                      (when (and (getf keyoption :optgroup)
                                 (getf keyoption :optname)
                                 (getf keyoption :showname))
                        keyoption)))
                keyoptions-list))))

(defmethod slots.%encode-to-string ((type (eql 'catalog-keyoptions)) keyoptions)
  (format nil "[~{~a~^,~}]"
          (mapcar #'(lambda (keyoption)
                      (format nil "{\"optgroup\":~a,\"optname\":~a,\"showname\":~a,\"units\":~a}"
                              (encode-json-to-string (getf keyoption :optgroup))
                              (encode-json-to-string (getf keyoption :optname))
                              (encode-json-to-string (getf keyoption :showname))
                              (encode-json-to-string (or (getf keyoption :units) ""))))
                  keyoptions)))

(defmethod slots.%decode-from-string ((type (eql 'catalog-keyoptions)) string)
  (declare (string string))
  (mapcar #'(lambda (pair)
              (list :optgroup (cdr (assoc :optgroup pair))
                    :optname (cdr (assoc :optname pair))
                    :showname (cdr (assoc :showname pair))
                    :units (cdr (assoc :units pair))))
          (decode-json-from-string string)))

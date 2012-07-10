;;;; slots.lisp

(in-package #:eshop)

(defun slots.string-delete-newlines (string)
  "Замена символов перевода каретки на пробел (актуально при сериализации имен групп)"
  (substitute #\Space #\Newline string))

(defgeneric slots.product-groups-fix (item)
  (:documentation "Remove given item from children lists, and
add it to such lists for its parents"))

(defmethod slots.product-groups-fix ((item product))
  "Remove given product from all groups' product lists and
add it to all parents' lists"
  ;;; TOCHECK
  (process-storage #'(lambda (group)
                       (setf (products group)
                             (remove (key item) (products group)
                                     :key #'key :test #'equal)))
                   'group)
  ;; add to parents
  (mapcar #'(lambda (group)
              (pushnew item (products group)))
          (parents item)))

(defmethod slots.product-groups-fix ((item group))
  "Remove given group from all groups' children lists and
add it to all parents' lists"
  ;;; TOCHECK
  (process-storage #'(lambda (group)
                       (setf (groups group)
                             (remove (key item) (groups group)
                                     :key #'key :test #'equal)))
                   'group)
  ;;add to parents
  (mapcar #'(lambda (group)
              (pushnew item (groups group)))
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

(defmethod slots.%encode-to-string (type value)
  (declare (ignore value))
  (error "Attempt to encode to string value of type, that hasn't string represenatation.
Type: ~A" type))

(defmethod slots.%decode-from-string (type string)
  (declare (ignore string))
  (error "Attempt to decode from string slot, that hasn't string reperesentation.
Type: ~A" type))

;;string - универсальный тип, так же используется как undefined
(defmethod slots.%view ((type (eql 'string)) value name disabled)
  (soy.class_forms:string-field
   (list :name name :disabled disabled :value value)))

(defmethod slots.%get-data ((type (eql 'string)) post-data-string)
  (declare (string post-data-string))
  post-data-string)

(defmethod slots.%encode-to-string ((type (eql 'string)) value)
  value)

(defmethod slots.%decode-from-string ((type (eql 'string)) string)
  (declare (string string))
  string)

;;int
(defmethod slots.%view ((type (eql 'int)) value name disabled)
  (slots.%view 'string (format nil "~D" value) name disabled))

(defmethod slots.%get-data ((type (eql 'int)) post-data-string)
  (declare (string post-data-string))
  (when (and (servo.valid-string-p post-data-string)
             (string/= "NIL" post-data-string))
    (parse-integer post-data-string)))

(defmethod slots.%encode-to-string ((type (eql 'int)) value)
  (format nil "~D" value))

(defmethod slots.%decode-from-string ((type (eql 'int)) string)
  (declare (string string))
  (parse-integer string))

;;textedit, онлайновый WYSIWYG редактор текста
(defmethod slots.%view ((type (eql 'textedit)) value name disabled)
  (if disabled
      (slots.%view 'string value name disabled)
      (soy.class_forms:texteditor
       (list :name name :value value))))

(defmethod slots.%get-data ((type (eql 'textedit)) post-data-string)
  "Replace #\Replace (#\Newline remains)"
  (servo.string-replace-chars post-data-string (list #\Return)))

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
(defmethod slots.%view ((type (eql 'textedit-hashtable)) value name disabled)
  (object-fields.string-field-view value name disabled))

(defmethod slots.%get-data ((type (eql 'textedit-hashtable)) string)
  (declare (string string))
  (object-fields.string-field-get-data string))

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
      (format nil "[~{~a~^,~}]" res-list))))

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
  (soy.class_forms:bool-field
   (list :name name :checked value :disabled disabled)))

(defmethod slots.%get-data ((type (eql 'bool)) post-data-string)
  (equal post-data-string "T"))

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
    (keys-to-objects (ensure-list string-list) :type 'group)))

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
   (list :keyoptions (let ((cnt 0))
                       (mapcar #'(lambda (keyoption)
                                   (incf cnt)
                                   (soy.class_forms:keyoption-field
                                    (append keyoption (list :number (- cnt 1)))))
                               value))
         :emptyfield (soy.class_forms:keyoption-field (list
                                                       :number (format nil "' + $~aCnt + '"
                                                                       name)))
         :name name
         :number (- (length value) 1)
         :disabled disabled)))


(defmethod slots.%get-data ((type (eql 'keyoptions)) post-data-string)
  ;;; see post-processing in admin.lisp
  ;;; TODO: make normal processing
  post-data-string)

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
   (list :keyoptions (let ((cnt 0))
                       (mapcar #'(lambda (keyoption)
                                   (incf cnt)
                                   (soy.class_forms:catalog-keyoption-field
                                    (append keyoption (list :number (- cnt 1)))))
                               value))
         :emptyfield (soy.class_forms:catalog-keyoption-field (list
                                                               :number (format nil "' + $~aCnt + '"
                                                                               (replace-all name "-" "_"))))
         :name (replace-all name "-" "_")
         :number (- (length value) 1)
         :disabled disabled)))

(defmethod slots.%get-data ((type (eql 'catalog-keyoptions)) string)
  string)

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

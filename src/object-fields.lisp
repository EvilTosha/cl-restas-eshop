(in-package #:eshop)

(defun object-fields.string-escaping (string)
  "Processing string and escapes quotes and backslashes"
  (let ((chars-for-escape (list #\\ #\")))
    (format nil "~{~a~}"
            (map 'list #'(lambda (char)
                           (if (notevery #'(lambda (char-for-escape)
                                             (char-not-equal char char-for-escape))
                                         chars-for-escape)
                               (format nil "\\~a" char)
                               (format nil "~a" char)))
                 string))))

(defun object-fields.string-replace-newlines (string)
  "Processing string and replace newline characters with #Newline"
	(regex-replace-all (format nil "~a" #\Return)
                     (regex-replace-all "\\n" string "#Newline")
                     ""))


(defun object-fields.string-delete-newlines (string)
  "Замена символов перевода каретки на пробел (актуально при сериализации имен групп)"
  (substitute #\Space #\Newline string))

(defun object-fields.string-add-newlines (string)
  "Adding newline characters instead of #Newline"
  (replace-all string "#Newline" (string #\Newline)))

(defun object-fields.serialize-optgroups (product-list filepath)
  (with-open-file (file filepath
                        :direction :output
                        :if-exists :supersede
                        :external-format :utf-8)
    (mapcar #'(lambda (product)
                (when (optgroups product)
                  (format file "{\"key\":~a, \"optgroups\":~a}~%"
                          (key product) (object-fields.optgroups-field-serialize
                                         (optgroups product)))))
            product-list)
    (format nil "Done!")))


(defmethod object-fields.product-groups-fix ((item product))
  ;;removing given product from all groups product lists
  (mapcar #'(lambda (group)
              (let ((products (copy-list (products group))))
                (setf (products group)
                      (remove-if #'(lambda (product)
                                     (equal (key product) (key item)))
                                 products))))
          (groups *global-storage*))
  ;;adding given product to parents' product lists
  (mapcar #'(lambda (group)
              (pushnew item (products group)))
          (parents item)))

(defmethod object-fields.product-groups-fix ((item group))
  ;;removing given group from all groups children lists
  (mapcar #'(lambda (group)
              (let ((groups (copy-list (groups group))))
                (setf (groups group)
                      (remove-if #'(lambda (group)
                                     (equal (key group) (key item)))
                                 groups))))
          (groups *global-storage*))
  ;;adding given product to parents' product lists
  (mapcar #'(lambda (group)
              (pushnew item (groups group)))
          (parents item)))


;;набор функций для показа, изменения и сериализации различных типов полей в админке
;;object-fields.*-field-view - просмотр
;;object-fields.*-field-get-data - декодирование из списка post-запроса
;;object-fields.*-field-serialize - сериализация поля
;;тип поля указывается в :type при создании класса (см. classes.lisp)

;;string - универсальный тип, так же используется как undefined
(defun object-fields.string-field-view (value name disabled)
  (soy.class_forms:string-field
   (list :name name :disabled disabled :value value)))

(defun object-fields.string-field-get-data (string)
  string)


(defun object-fields.string-field-serialize (string)
  (format nil "\"~a\"" (object-fields.string-escaping string)))

;;int
(defun object-fields.int-field-view (value name disabled)
  (object-fields.string-field-view (format nil "~a" value) name disabled))

(defun object-fields.int-field-get-data (string)
  (when (and string (string/= "" string) (string/= "NIL" string))
    (parse-integer string)))

(defun object-fields.int-field-serialize (int)
  (encode-json-to-string int))


;;textedit, онлайновый WYSIWYG редактор текста
(defun object-fields.textedit-field-view (value name disabled)
  (if disabled
      (object-fields.string-field-view value name disabled)
      (soy.class_forms:texteditor
       (list :name name :value value))))

(defun object-fields.textedit-field-get-data (string)
	"Replace #\Replace (#\Newline remains)"
  (servo.string-replace-chars string (list #\Return)))

(defun object-fields.textedit-field-serialize (text)
  (object-fields.string-field-serialize (object-fields.string-replace-newlines text)))


;;textedit-raw, текстовое поле для правки кода
(defun object-fields.textedit-raw-field-view (value name disabled)
  (if disabled
      (object-fields.string-field-view value name disabled)
      (soy.class_forms:texteditor-raw
       (list :name name :value value))))

(defun object-fields.textedit-raw-field-get-data (string)
	"Replace #\Replace (#\Newline remains)"
  (servo.string-replace-chars string (list #\Return)))

(defun object-fields.textedit-raw-field-serialize (text)
  (object-fields.string-field-serialize (object-fields.string-replace-newlines text)))


;;textedit-hashtable, hashtable of texteditfields
(defun object-fields.textedit-hashtable-field-view (value name disabled)
  (object-fields.string-field-view value name disabled))

(defun object-fields.textedit-hashtable-field-get-data (string)
  (object-fields.string-field-get-data string))

(defun object-fields.textedit-hashtable-field-serialize (hashtable)
  (let ((res-list))
    (maphash #'(lambda (vendor seo-text)
                 (push
                  (format nil "~a,~a"
                          (encode-json-to-string vendor)
                          (object-fields.textedit-field-serialize seo-text))
                  res-list))
             hashtable)
    (when res-list
      (format nil "[~{~a~^,~}]" res-list))))

;;time, человекопонятное время
(defun object-fields.time-field-view (value name disabled)
  (object-fields.string-field-view (time.decode-date-time value) name disabled))

(defun object-fields.time-field-get-data (string)
  string)

(defun object-fields.time-field-serialize (time)
  (object-fields.int-field-serialize time))


;;bool
(defun object-fields.bool-field-view (value name disabled)
  (soy.class_forms:bool-field
   (list :name name :checked value :disabled disabled)))

(defun object-fields.bool-field-get-data (string)
  (string= string "T"))

(defun object-fields.bool-field-serialize (bool)
  (encode-json-to-string bool))


;;генерация дерева групп
;;open - список групп до которых нужно раскрыть дерево
;;field-name - название поля, нужно для проставления в name в radio
(defun object-fields.group-tree (open-groups field-name)
  (let ((roots (root-groups *global-storage*)))
    (soy.class_forms:group-tree
     (list :roots
           (mapcar #'(lambda (child)
                       (object-fields.group-branch child open-groups field-name))
                   roots)))))


;;group, список групп, генерируется из списка с проставленными уровнями глубины
(defun object-fields.group-list-field-view (value name disabled)
  (soy.class_forms:group-field (list :name name
                                     :tree (object-fields.group-tree value name)
                                     :disabled disabled)))

;;генерация ветви дерева с корнем в данной группе
;;group - корень ветви, open-groups - список групп, до которых нужно раскрыть дерево
;;field-name - имя поля, нужно для проставления в name в radio
(defun object-fields.group-branch (group open-groups field-name)
  (let ((open nil)
        (child-open nil)
        (children)
        (checked nil))
    ;; (log5:log-for test "~&GROUP:~a ~{~a~}" (key group) (groups group))
    ;;выясняем нужно ли открывать группу
    (mapcar #'(lambda (open-group)
                (when (eq (key group) (key open-group))
                    (setf open t)
                    (setf checked t)))
            open-groups)
    ;;строим список дочерних ветвей и проверяем нужно ли открыть данный узел
    (setf children
          (mapcar #'(lambda (child)
                      (multiple-value-bind (branch branch-opened)
                          (object-fields.group-branch child open-groups field-name)
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


(defun object-fields.group-list-field-get-data (string-list)
  (when string-list
    (if (equal (type-of string-list) 'cons)
        (mapcar #'(lambda (parent)
                    (log5:log-for debug-console "~a~%" parent)
                    (gethash parent (storage *global-storage*)))
                string-list)
        (list (gethash string-list (storage *global-storage*))))))


(defun object-fields.group-list-field-serialize (groups)
  (format nil "[~{\"~a\"~^,~}]"
          (mapcar #'(lambda (group)
                      (key group))
                  groups)))

;;optgroups
(defun object-fields.optgroups-field-view (value name disabled)
  (object-fields.string-field-view value name disabled))

(defun object-fields.optgroups-field-get-data (string)
  (object-fields.string-field-get-data string))

(defun object-fields.optgroups-field-serialize (optgroups)
  (let ((entity
         (remove-if #'null
                    (mapcar #'(lambda (optgroup)
                                (let ((name (getf optgroup :name))
                                      (options (remove-if #'null
                                                          (mapcar #'(lambda (option)
                                                                      (let ((name (getf option :name))
                                                                            (value (getf option :value)))
                                                                        (when (and name value (string/= name "") (string/= value "") (string/= name "NIL"))
                                                                          (format nil "{\"name\":\"~a\",\"value\":\"~a\"}"
                                                                                  name (object-fields.string-replace-newlines (object-fields.string-escaping value))))))
                                                                  (getf optgroup :options)))))
                                  (when (and name options (string/= name "") (string/= name "NIL"))
                                    (format nil "{\"name\":\"~a\",\"options\":[~{~a~^,~}]}"
                                            name options))))
                            optgroups))))
    (if entity
      (format nil "[~{~a~^,~}]" entity)
      "null")))



;;products, список продуктов(-детей)
(defun object-fields.product-list-field-view (value name disabled)
  (object-fields.string-field-view value name disabled))


(defun object-fields.product-list-field-get-data (string-list)
  (if (equal (type-of string-list) 'cons)
      (mapcar #'(lambda (parent)
                  (log5:log-for debug-console "~a~%" parent)
                  (gethash parent (storage *global-storage*)))
              string-list)
      (list (gethash string-list (storage *global-storage*)))))

(defun object-fields.product-list-field-serialize (products)
  (format nil "[~{\"~a\"~^,~}]"
          (mapcar #'(lambda (product)
                      (key product))
                  products)))


;;keyoptions
(defun object-fields.keyoptions-field-view (value name disabled)
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


(defun object-fields.keyoptions-field-get-data (string)
  string)
  ;; (mapcar #'servo.alist-to-plist (decode-json-from-string string)))

(defun object-fields.keyoptions-field-serialize (keyoptions)
  (format nil "[~{~a~^,~}]"
          (mapcar #'(lambda (keyoption)
                           (format nil "{\"optgroup\":~a,\"optname\":~a}"
                                   (encode-json-to-string (getf keyoption :optgroup))
                                   (encode-json-to-string (getf keyoption :optname))))
                  keyoptions)))


(defun object-fields.catalog-keyoptions-field-view (value name disabled)
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

(defun object-fields.catalog-keyoptions-field-get-data (string)
   string)

(defun object-fields.catalog-keyoptions-field-serialize (keyoptions)
  (format nil "[~{~a~^,~}]"
          (mapcar #'(lambda (keyoption)
                      (format nil "{\"optgroup\":~a,\"optname\":~a,\"showname\":~a,\"units\":~a}"
                              (encode-json-to-string (getf keyoption :optgroup))
                              (encode-json-to-string (getf keyoption :optname))
                              (encode-json-to-string (getf keyoption :showname))
                              (encode-json-to-string (or (getf keyoption :units) ""))))
                  keyoptions)))

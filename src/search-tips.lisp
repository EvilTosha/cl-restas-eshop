;; TODO: move to own package
(in-package #:eshop)

(defclass search-tip ()
  ((tip :accessor tip :initarg :tip :initform "")
   (weight :accessor weight :initarg :weight :initform 0)))

(defmethod print-object ((object search-tip) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "tip ~A with weight ~A" (tip object) (weight object))))

(defclass search-tips ()
  ((interval-tree :accessor interval-tree :initform #())
   (tips :accessor tips :initarg :tips :initform #())))

(defmethod print-object ((object search-tips) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "search-tips count ~A" (length (tips object)))))

;; TODO: write post-creating processig for interval tree

(defun tips-elt (tips index)
  (declare (search-tips tips) (fixnum index))
  (elt (tips tips) index))

(defun (setf tips-elt) (val tips index)
  (declare (search-tip val) (search-tips tips) (fixnum index))
  (setf (elt (tips tips) index) val))

(defun it-elt (tips index)
  (declare (search-tips tips) (fixnum index))
  (elt (interval-tree tips) index))

(defun (setf it-elt) (val tips index)
  (declare (search-tips tips) (fixnum val index))
  (setf (elt (interval-tree tips) index) val))

(defun it-son (v vl vr direction)
  "Retrurns list of 3 values: index of son of specified vertex in interval tree, and right andd left bounds for its interval.
Aceepted values of direction are :left and :right. If other direction
parameter specified, :left is used.
If vertex is a leaf, return nil"
  (declare (fixnum v vl vr) (keyword direction))
  (unless (= vl vr)
    (let ((mid (floor (alexandria:mean (list vl vr)))))
      (if (equal direction :right)
          (list (* 2 v) vl mid)
          (list (1+ (* 2 v)) (1+ mid) vr)))))

(defun it-max (tips ind1 ind2)
  "By given 2 indexes, compare corresponding values and return index with larger value.
Value for negative indexes is -infinity"
  (declare (search-tips tips) (fixnum ind1 ind2))
  (cond
    ((minusp ind1) ind2)
    ((minusp ind2) ind1)
    (t (if (< (weight (tips-elt tips ind1)) (weight (tips-elt tips ind2)))
           ind2
           ind1))))

(defun %build-interval-tree (tips v vl vr)
  "Init max in current vertex and go recursively down to children"
  (declare (search-tips tips) (fixnum v vl vr))
  (if (= vl vr)
      (setf (it-elt tips v) vl)
      (let ((left-son (it-son v vl vr :left))
            (right-son (it-son v vl vr :right)))
        (apply #'%build-interval-tree tips left-son)
        (apply #'%build-interval-tree tips right-son)
        (setf (it-elt tips v) (it-max tips (it-elt tips (first left-son))
                                      (it-elt tips (first right-son)))))))

(defun build-interval-tree (tips)
  "By given array of weighted search tips build interval tree for maximums."
  (declare (search-tips tips))
  (%build-interval-tree tips 1 0 (- (length (tips tips)) 1)))

(defun build-search-tips (tips)
  "Updates *search-tips* variable with new values.
Be careful: old values will be lost.
Returns created instance."
  (declare (array tips))
  (let ((new-tips (make-instance 'search-tips)))
    (setf (tips new-tips) (sort tips #'string< :key #'tip)
          (interval-tree new-tips) (make-array (* 4 (length tips)) :element-type 'fixnum
                                               :initial-element most-negative-fixnum))

    (build-interval-tree new-tips)
    new-tips))

(defun %get-it-max (tips v vl vr request-l request-r)
  "Returns index of maximum on request interval"
  (declare (search-tips tips) (fixnum v vl vr request-l request-l))
  (cond
    ((or (> request-l vr) (< request-r vl)) most-negative-fixnum)
    ((and (= vl request-l) (= vr request-r)) (it-elt tips v))
    (t (let ((mid (floor (+ vl vr) 2)))
         (it-max tips
                 (%get-it-max tips (* 2 v)       vl        mid  request-l                (min request-r mid))
                 (%get-it-max tips (1+ (* 2 v))  (1+ mid)  vr   (max request-l (1+ mid)) request-r))))))

(defun get-it-max (tips request-l request-r)
  "Returns index of maximum on requested interval"
  (declare (search-tips tips) (fixnum request-l request-r))
  (%get-it-max tips 1 0 (- (length (tips tips)) 1) request-l request-r))

(defun binary-lower-bound (array elt &key (key #'identity) (comp #'<))
  (declare (array array) (function comp))
  (let ((l 0) (r (length array)))
    (loop :while (< l r)
       :do (let ((m (floor (+ l r) 2)))
             (if (funcall comp (funcall key (elt array m)) elt)
                 (setf l (1+ m))
                 (setf r m)))
       :finally (return l))))

(defun next-prefix (prefix)
  "Returns next (to current) lexicographical prefix, by increasing char-code of last char by 1"
  (declare (string prefix))
  (when (plusp (length prefix))
    (let ((res prefix)
          (last-index (- (length prefix) 1)))
      (setf (char res last-index)
            (code-char (1+ (char-code (char res last-index)))))
      res)))

(defun max-tip-by-prefix (tips prefix)
  "Returns single tip with given prefix and with maximum weight"
  (declare (search-tips tips) (string prefix))
  (let* ((l (binary-lower-bound (tips tips) prefix :comp #'string< :key #'tip))
         (r (binary-lower-bound (tips tips) (next-prefix prefix) :comp #'string< :key #'tip))
         (index (get-it-max tips l (- r 1))))
    (unless (minusp index)
      (tips-elt tips index))))


;; ??: is it really needed?
(defun nearest-degree-of-two (n)
  "Finds nearest (greater than n) degree of 2"
  (declare (integer n))
  (loop :for x := 1 :then (* x 2)
     :while (< x n)
     :finally (return x)))

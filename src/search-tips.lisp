;; TODO: move to own package
(in-package #:eshop)

(defclass search-tip ()
  ((tip :accessor tip :initarg :tip :initform "")
   (weight :accessor weight :initarg :weight :initform 0)))

(defclass search-tips ()
  ((interval-tree :accessor interval-tree :initform #())
   (tips :accessor tips :initarg :tips :initform #())))

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

;; TODO: specify 'search-tips variable to work with
;; (don't use global one)
(defun %build-interval-tree (tips v vl vr)
  "Init max in current vertex and go recursively down to children"
  (declare (search-tips tips) (fixnum v vl vr))
  (if (= vl vr)
      (setf (it-elt tips v) vl)
      (progn
        (let ((left-son (it-son v vl vr :left))
              (right-son (it-son v vl vr :right)))
          (apply #'%build-interval-tree tips left-son)
          (apply #'%build-interval-tree tips right-son)
          (let ((left-val (weight (tips-elt tips (it-elt tips (first left-son)))))
                (right-val (weight (tips-elt tips (it-elt tips (first right-son))))))
            (setf (it-elt tips v) (if (< left-val right-val)
                                      (it-elt tips (first right-son))
                                      (it-elt tips (first left-son)))))))))

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
    (setf (tips new-tips) (sort tips #'< :key #'weight)
          (interval-tree new-tips) (make-array (* 4 (length tips)) :element-type 'fixnum
                                               :initial-element most-negative-fixnum))

    (build-interval-tree new-tips)
    new-tips))

(defun %get-it-max (tips v vl vr request-l request-r)
  "Returns maximum on request interval"
  (declare (search-tips tips) (fixnum v vl vr request-l request-l))
  (cond
    ((> request-l request-r) most-negative-fixnum)
    ((and (= vl request-l) (= vr request-r)) (it-elt tips v))
    (t (let ((mid (floor (alexandria:mean (list vl vr)))))
         (max (%get-it-max tips (* 2 v)       vl        mid  request-l                (min request-r mid))
              (%get-it-max tips (1+ (* 2 v))  (1+ mid)  vr   (max request-l (1+ mid)) request-r))))))

(defun get-it-max (tips request-l request-r)
  "Returns maximum on requested interval"
  (declare (search-tips tips) (fixnum request-l request-r))
  (%get-it-max tips 1 0 (- (length (tips tips)) 1) request-l request-r))

(defun get-max-on-whole-interval (tips)
  (declare (search-tips tips))
  (it-elt tips 1))

(defun get-max-weight-tip (tips)
  (declare (search-tips tips))
  (tips-elt tips (get-max-on-whole-interval tips)))


;; ??: is it really needed?
(defun nearest-degree-of-two (n)
  "Finds nearest (greater than n) degree of 2"
  (declare (integer n))
  (loop :for x := 1 :then (* x 2)
     :while (< x n)
     :finally (return x)))

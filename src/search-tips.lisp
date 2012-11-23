;; TODO: move to own package
(in-package #:eshop)

(defclass search-tip ()
  ((tip :accessor tip :initarg :tip :initform "")
   (weight :accessor weight :initarg :weight :initform 0)))

(defclass search-tips ()
  ((interval-tree :accessor interval-tree :initform #())
   (tips :accessor tips :initarg :tips :initform #())))

;; TODO: write post-creating processig for interval tree

(defparameter *search-tips* (make-instance 'search-tips))

(defun it-son (v vl vr direction)
  "Retrurns list of 3 values: index of son of specified vertex in interval tree, and right andd left bounds for its interval.
Aceepted values of direction are :left and :right. If other direction
parameter specified, :left is used.
If vertex is a leaf, return nil"
  (declare (integer v vl vr) (keyword direction))
  (unless (= vl vr)
    (let ((mid (alexandria:mean (list vl vr))))
      (if (equal direction :right)
          (list (* 2 v) vl mid)
          (list (1+ (* 2 v)) (1+ mid) vr)))))

;; TODO: specify 'search-tips variable to work with
;; (don't use global one)
(defun %build-interval-tree (tips v vl vr)
  "Init max in current vertex and go recursively down to children"
  (declare (search-tips tips) (integer v vl vr))
  (if (= vl vr)
      (setf (elt (interval-tree tips) v) (weight (elt (tips tips) vl)))
      (progn
        (apply #'%build-interval-tree tips (it-son v vl vr :left))
        (apply #'%build-interval-tree tips (it-son v vl vr :right)))))

(defun build-interval-tree (tips)
  "By given array of weighted search tips build interval tree for maximums."
  (declare (search-tips tips))
  (%build-interval-tree tips 1 0 (- (length tips) 1)))

(defun build-search-tips (tips)
  "Updates *search-tips* variable with new values.
Be careful: old values will be lost.
Returns created instance."
  (declare (array tips))
  (let ((new-tips (make-instance 'search-tips)))
    (setf (tips new-tips) (sort tips #'< :key #'weight))
    (build-interval-tree new-tips)
    new-tips))


;; ??: is it really needed?
(defun nearest-degree-of-two (n)
  "Finds nearest (greater than n) degree of 2"
  (declare (integer n))
  (loop :for x := 1 :then (* x 2)
     :while (< x n)
     :finally (return x)))

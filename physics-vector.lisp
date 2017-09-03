;;macro to easily define per-element-operations for lm:vector
(defmacro vector-element-function (function vector-function-name)
  `(defun ,vector-function-name (vector)
     (declare (type lm:vector vector))
     (let ((dim (lm:dimension vector)))
       (lm:make-vector dim :initial-elements 
		       (loop for x from 0 below dim collect 
			    (,function (lm:elt vector x)))))))

;;macro to easily define per-element-operations for lm:vector
(defmacro two-vector-element-function (function vector-function-name)
  `(defun ,vector-function-name (vector vectorb)
     (declare (type lm:vector vector) (type lm:vector vectorb))
     (let ((dim (lm:dimension vector)))
       (lm:make-vector dim :initial-elements 
		       (loop for x from 0 below dim collect 
			    (,function 
			     (lm:elt vector x) 
			     (lm:elt vectorb x))))
       )))

;;macro to easily define per-element-operations for lm:vector
(defmacro four-vector-element-function (function vector-function-name)
  `(defun ,vector-function-name (a b c d)
     (declare (type lm:vector a) (type lm:vector b))
     (let ((dim (lm:dimension a)))
       (lm:make-vector dim :initial-elements 
		       (loop for x from 0 below dim collect 
			    (,function 
			     (lm:elt a x) 
			     (lm:elt b x)
			     (lm:elt c x) 
			     (lm:elt d x)
			     )))
       )))


(two-vector-element-function random-range random-vector)
(vector-element-function abs vector-abs)
(vector-element-function float-sign vector-sign)


(defun only-positive (value)
  (if (> 0 value)
      0
      value))

(vector-element-function only-positive vector-only-positive)

(defun vector-min (vector)
  (reduce #'min (slot-value vector 'lm::data)))

(defun vector-max (vector)
  (reduce #'max (slot-value vector 'lm::data)))

(defun set-vector (vector value)
  (let ((out (lm:* 1d0 vector)))
    (loop for x from 0 below (lm:dimension vector) do
	 (setf (lm:elt out x) value))
    out))


(defun vector-min-indexed (vector)
  (let ((min most-positive-double-float) (idx 0))
    (let ((dim (lm:dimension vector))
	  (data (slot-value vector 'lm::data)))
      (loop for x from 0 below dim do
	   (let ((value (elt data x)))
	     (when (< value min)
	       (setf min value)
	       (setf idx x)))))
    (values min idx)))

(defun vector-max-indexed (vector)
  (let ((max most-negative-double-float) (idx 0))
    (let ((dim (lm:dimension vector))
	  (data (slot-value vector 'lm::data)))
      (loop for x from 0 below dim do
	   (let ((value (elt data x)))
	     (when (> value max)
	       (setf max value)
	       (setf idx x)))))
    (values max idx)))
	 
(defun min-or-zero (vector)
  (let ((min (vector-min vector))(dim (lm:dimension vector)))
    
    (lm:make-vector dim 
		    :initial-elements (map 'list 
					  (lambda (x) (if (equal x min) x 0d0)) 
					  (slot-value vector 'lm::data)))))

(defun max-or-zero (vector)
  (let ((max (vector-max vector))(dim (lm:dimension vector)))
    
    (lm:make-vector dim 
		    :initial-elements (map 'list 
					  (lambda (x) (if (equal x max) x 0d0)) 
					  (slot-value vector 'lm::data)))))



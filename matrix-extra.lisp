(defpackage :matrix-extra (:use cl)
	    (:export :vector-distance :rotate-vector
		     :with-vector-reader)
	    (:nicknames :mex))
(in-package :matrix-extra)

(defun rotate-vector (vec r)
  (let ((cosr (cos r))
	(sinr (sin r))
	(x (m:ref vec 0))
	(y (m:ref vec 1)))
    (m:vec (- (* x cosr) (* y sinr))
	   (+ (* x sinr) (* y cosr)))))

(defun vector-distance (vec)
  (sqrt (m:ref (matrix:dot (matrix:transpose vec) vec ) 0)))

(defmacro with-vector-reader (name vec &rest body)
  (let ((sym (gensym)))
    `(let ((,sym ,vec))
       
       (flet ((,name (idx)
		(m:ref ,sym idx))
	      ((setf ,name) (value idx)
		(setf (m:ref ,vec idx) value)))
	 ,@body))))


;; ** tests ** ;;
(defun test-rotate-vector()
  "subtract pi rotated vectors to give 0.0 distance"
  (flet ((test-pi-rot (x y)
	   (unless (< (vector-distance (m:+ (rotate-vector (m:vec x y) pi) (m:vec x y))) 0.0001 )
	     (error "unexpected value"))))
    (loop for x from -5.0 to 5.0 by 1.0 do
	 (loop for y from -5.0 to 5.0 by 1.0 do
	      (test-pi-rot x y)))))

(test:register 'test-rotate-vector)


 (defun test-with-vector-reader ()
   (let ((vec (m:vec 1.0 2.0 3.0 4.0)))
     (with-vector-reader vec vec 
			 
       (unless (and (eq (vec 0) 1.0) (eq (vec 1) 2.0)
		    (eq (vec 2) 3.0) (eq (vec 3) 4.0))
	 (error "Unexpected value"))
       (setf (vec 0) 5.0))
     (unless (eq (m:ref vec 0) 5.0)
       (error "didnt set value"))))
(test:register 'test-with-vector-reader)
				       

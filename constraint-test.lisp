(re.load "nd-tree.lisp")
(defmacro defconstraint (constraint-name base-constraints args)
 "")

(defconstraint point () ())
(defconstraint circle (point) (radius))
(defconstraint tree (circle) (height))
(defconstraint player (circle) ())
(defun add-constraint (constraint-map x y constraint)
  ())

(defun get-nearby-constraints (constraint-map x y distance loc)
  ())

;(defun update-constraints (constraint-map x y distance lod)

;(defun constraint-example ()
;  (let ((constraint-map nil))
(defun 1d-rnd-to-2d (1d-value)
  (let ((sum (make-vector 0 0)))
    (loop for x from 0 to 9 do
	 (when (> 0.25 1d-value)
	   (setf sum (lm:* (/ (- 9 x) 10) (make-vector (if (> 1d-value 0.50) 1.0 0.0) (if (> 1d-value 0.25) 1.0 0.0)))))
	 (print sum))

	 
    sum))

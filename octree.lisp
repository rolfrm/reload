
(defclass octree-ref ()
  ((parent :initform nil :initarg :parent)
   (idx :initform 0 :initarg :idx)
   (octree-data :initarg :tree)
   (octree-offset :initarg :offset)))

(defun convert-type (type-value)
  (case type-value
    (0 :null)
    (1 :octptr8)
    (2 :octptr16)
    (3 :octptr32)
    (4 :octptr64)
    (6 :data32)
    (otherwise (error "Type not implemented"))))
   
(defun v3map (func first &rest rest-vectors)
  (apply #'map '(vector number 3) func first rest-vectors))

(defun offset-to-idx (v)
  (reduce #'+ (v3map #'* v #(1 2 4))))

(defmethod get-parent ((self octree-ref))
  (with-slots (parent) self
    parent))

(defmethod get-child-idx ((self octree-ref) child-idx)
  (with-slots (parent idx octree-data octree-offset) self
    (make-instance 'octree-ref :parent self 
		   :idx child-idx
		   :tree octree-data
		   :offset 
		   (+ octree-offset 
		      (aref octree-data
			    (+ octree-offset child-idx 1))))))

(defmethod get-child ((self octree-ref) vec)
  (let ((new-octree 
	 (get-relative self (v3map (lambda (x) (ash x -1)) vec)))
	(idx (offset-to-idx (v3map (lambda (x) (logand x 1)) vec))))
    (get-child-idx new-octree idx)))
(defmethod get-type ((self octree-ref))
  (with-slots (octree-offset octree-data) self
    (convert-type (aref octree-data octree-offset))))

(defmethod get-idx ((self octree-ref))
  (with-slots (idx) self
    idx))

(defmethod get-pos ((self octree-ref))
  (let ((idx (get-idx self)))
    (vector (logand idx 1) 
	    (logand (ash idx -1) 1) 
	    (logand (ash idx -2) 1))))

(defmethod get-relative((self octree-ref) vec)
  (if (every (lambda (x)(equal x 0)) vec)
      self
      (multiple-value-call #'get-child (move-to-parent self vec))))

;(defmethod get-value ((self octree-ref))
;  (error "Needs implementation"))

;(defmethod set-value ((self octree-ref) value)
;  (error "Needs implementation"))

(defmethod move-to-parent ((self octree-ref)  pos)
  (let ((parent (get-parent self)))
    (unless parent
      (error "move-to-parent: Parent is nil"))
    (let ((cell-pos (get-pos self)))
    (values parent 
	    (v3map 
	     (lambda (p cell-p)
	       (+ cell-p (/ p 2))) pos cell-pos)))))

(defparameter testoctree 
  (make-array 49 :initial-contents 
	      '(1 9 14 19 24 29 34 39 44 
		6 1.0 0.0 0.0 1.0 
		6 1.0 0.0 0.0 1.0 
		6 1.0 0.0 1.0 1.0 
		6 0.0 1.0 1.0 1.0

		6 0.5 1.0 1.0 1.0 
		6 0.0 0.5 1.0 1.0
		6 0.0 1.0 1.0 1.0 
		6 0.5 0.5 1.0 1.0)))
(defparameter oct (make-instance 'octree-ref :idx 0 :parent nil :tree testoctree :offset 0))

(defun test-oct ()
  (progn
    (assert (equal oct (get-parent (get-child oct #(0 0 0)))))
    (assert (equal 7 (slot-value (get-child oct #(1 1 1)) 'idx)))))
(test-oct)

(defun find-starting-point( octree-ref camera-pos)
  (if (and (get-parent octree-ref) 
	   (or (notevery (lambda (x) (>= x 0)) camera-pos)
	       (notevery (lambda (x) (<= x 1)) camera-pos)))
      (multiple-value-call #'find-starting-point (move-to-parent octree-ref camera-pos ))
      (values camera-pos octree-ref)))

(defun line.line.projection (line-start line-stop static-axes varying-axis-nr)
  (let* ((other-axes (remove varying-axis-nr '(0 1 2)))
	 (first-axis (first other-axes))
	 (snd-axis (second other-axes))
	 
	 (px.1 (nth first-axis line-start))
	 (px.2 (nth first-axis line-stop))
	 (pex(first static-axes))
	 (py.1 (nth snd-axis line-start))
	 (py.2 (nth snd-axis line-stop))
	 (pey (second static-axes)))
    (let ((r (/ 
	      (- (* pex py.2) (* px.2 pey)) 
	      (+ (* pey px.1) (* py.1 pey -1) 
		 (* pex py.1) (* pex py.2)))))
      (print r))))
	 

(defun line.line.projection2(line-start line-stop static-axes varying-axis-nr)
  (let* ((other-axes (remove varying-axis-nr '(0 1 2)))
	 (first-axis (first other-axes))
	 (snd-axis (second other-axes))
	 
	 (px.2 (nth first-axis line-start))
	 (px.1 (nth first-axis line-stop))
	 (pex(first static-axes))
	 (py.2 (nth snd-axis line-start))
	 (py.1 (nth snd-axis line-stop))
	 (pey (second static-axes)))
    (/ (- 
	(print (/ pex (- px.1 px.2))) 
	(print (/ pey (- py.1 py.2))))
       (- 
	(print (/ px.2 (- px.1 px.2))) 
	(print (/ py.2 (- py.1 py.2)))))))

(defun llp3(line-start line-stop static-axes varying-axis-nr)
  (let* ((other-axes (remove varying-axis-nr '(0 1 2)))
	 (first-axis (first other-axes))
	 (snd-axis (second other-axes))
	 
	 (px.2 (nth first-axis line-start))
	 (px.1 (nth first-axis line-stop))
	 (pex(first static-axes))
	 (py.2 (nth snd-axis line-start))
	 (py.1 (nth snd-axis line-stop))
	 (pey (second static-axes)))
    (print (format nil "~a ~a ~a ~a ~a ~a" px.1 px.2 pex py.1 py.2 pey))
    (/ (print (- (* pex py.2) (* pey px.2)))
       (print (- (* pey (- px.1 px.2)) (* pex (- py.1 py.2)))))))



;(defun render-voxel-octree (screen-size camera-pos camera-dir
;			    aspect-ratio octree-ref)
 ; 
 ; ((let ((cam-pos
  
  

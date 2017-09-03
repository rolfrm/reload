
(defpackage nd-tree (:use :cl :utils  :with-package :alexandria))

(in-package :nd-tree)

(utils:ex>

(defmacro nd-tree-create (number-of-dimensions name)
  
  (let* ((number-of-cells (ash 1 number-of-dimensions))
	(all-dim-names '(x y z w u v s t))
	(dim-names (take all-dim-names number-of-dimensions))
	 (dim-names-key-list (loop for dim in dim-names collect (list dim 0))))
    `(progn 
       (labels ((parent-index-from-index (index)
		  (logand (lognot index) (- ,number-of-cells 1)))
		(dims-to-index ,dim-names 
		  (+ ,@(loop for x from 0 below (length dim-names) for dim in dim-names 
			  collect `(ash ,dim ,x))))
		(dims-valid-child ,dim-names
		  (and ,@(loop for dim in dim-names collect `(or (eq ,dim 0) (eq ,dim 1)))))
		(dims-all-zero ,dim-names
		  (and ,@(loop for dim in dim-names collect `(eq 0 ,dim))))
		)
						
	 (defclass ,name ()
	   ((children :initform nil :accessor nd-tree-children)
	  (payload :initform nil :accessor payload)
	  (parent :initarg :parent :initform nil)
	  (index :initarg :index :initform 0 :accessor nd-tree-index)))

	 (defmethod create-children ((nd-tree ,name))
	   (with-slots (children) nd-tree
	     (if children
		 children
		 (progn
		   (setf children (make-array (n-cells nd-tree)))
		   (loop for iter from 0 below (n-cells nd-tree) do
			(setf (aref children iter) (make-instance ',name :parent nd-tree :index iter)))
		   children))))

	 (defmethod n-cells ((nd-tree ,name))
	   ,number-of-cells)

	 (defmethod dimensionality ((nd-tree ,name))
	   ,number-of-dimensions)

	 (defmethod split ((nd-tree ,name))
	   (with-slots (children) nd-tree
	     (if children
		 children
		 (create-children nd-tree))))

	 (defmethod create-parent ((nd-tree ,name))
	   (with-slots (parent index) nd-tree
	     (unless parent
	       (setf parent (make-instance ',name :index (parent-index-from-index index)))
	       (setf (aref (create-children parent) index) nd-tree)
	       )
	     parent))
	 (defmethod ensure-parent ((nd-tree ,name) &key (index-of-created-parent -1))
	   (with-slots (parent index) nd-tree
	     (unless parent
	       (setf parent (make-instance ',name :index 
					   (if (> index-of-created-parent -1)
					       (if (in-range #'<= #'> 0 
								 ,number-of-cells 
								 index-of-created-parent)
						   index-of-created-parent
						   (error "Invalid index for tree node"))
					       (parent-index-from-index index))))
	       (setf (aref (create-children parent) index) nd-tree)
	       )
	     parent))



	 (defmethod _get-child ((nd-tree ,name) idx)
	   (if (in-range #'<= #'> 0 ,number-of-cells idx)
	       (with-slots (children) nd-tree
		 (if children
		     (aref children idx)
		     (error "No children created")))
	       (error "Invalid index")))

	 (defmethod get-child ((nd-tree ,name) &key ,@dim-names-key-list (idx -1))
	   (_get-child 
	    nd-tree 
	    (if (> idx -1)
		idx
		(dims-to-index ,@dim-names))))
	    
	 (defmethod (setf get-child) (value (nd-tree ,name) &key ,@dim-names-key-list (idx -1))
	   (let ((actual-idx (if (> idx -1) idx (dims-to-index ,@dim-names))))
	     (with-slots (children)
		 (setf (aref children actual-idx) value))))

	 (defmethod get-relative-child ((nd-tree ,name) &key ,@dim-names-key-list)
	   (if (dims-valid-child ,@dim-names)
	       (progn
		 (create-children nd-tree)
		 (get-child nd-tree :idx (dims-to-index ,@dim-names)))
	       (let ((rel-node (get-relative-node nd-tree ,@(loop for dim in dim-names append (list (symbol-to-keyword dim) `(ash ,dim -1))))))
		 (create-children rel-node)
		 (get-child rel-node ,@(loop for dim in dim-names append (list (symbol-to-keyword dim) `(logand ,dim 1)))))))
	       

	 (defmethod get-relative-node ((nd-tree ,name) &key ,@dim-names-key-list)
	   (with-slots (index) nd-tree
	     
	     (if (dims-all-zero ,@dim-names)
		 nd-tree
		 (get-relative-child (create-parent nd-tree) ,@(loop for dim in dim-names
								  for x from 0 below (length dim-names) 
								  append 
								    (list (symbol-to-keyword dim) (list '+ dim (list 'logand (list 'ash 'index (- x)) 1))))))))
	 
   ))
    ))

(nd-tree-create 3 octree)
(nd-tree-create 2 quadtree)

(defun test-quadtree ()
  (let* ((child (make-instance 'quadtree :index 0))
	 (parent (create-parent child)))
    (print parent)
    (print child)
    (print (slot-value parent 'children))))

(defun test-quadtree2 ()
  (let* ((child (make-instance 'quadtree :index 0))
	 (from-value -10) 
	 (to-value 10))
    (loop for x from from-value below to-value do
	 (loop for y from from-value below to-value do
	      (setf (payload (get-relative-node child :x x :y y))
		    (+ y (* (- to-value from-value) x)))))
    (loop for x from from-value below to-value do
	 (loop for y from from-value below to-value do
	      (let ((val (payload (get-relative-node child :x x :y y)))
		    (tst (+ y (* (- to-value from-value) x))))
		(printf "~a ~a ~a ~a ~a" x y val tst (eql val tst)))))))
	
(defun avg (sample)
  (if sample
      (/ (apply #'+ sample) (length sample))
      0))

(defun test-quadtree3 (&key (range 8) (neg-range 0))
  (let ((child (make-instance 'quadtree :index 0)))
    (labels ((get-relative-parent (tree x y) (get-relative-node (create-parent tree) :x x :y y))
	     (iterate (tree it)
	       (progn
		 (loop for x from neg-range below range collect
			     (loop for y from neg-range below range collect
				  (let ((parent (get-relative-parent tree x y)))
				    (setf (payload parent) 
					  (let ((average (avg (remove-if #'null (map 'list (lambda (child) (payload child)) (nd-tree-children parent))))))
					    (if (> average 0)
						1.0
						0.0))))))
		 (when (< 0 it)
		   (iterate (create-parent tree) (- it 1))))))
		     

      (loop for x from neg-range below range collect
	   (loop for y from neg-range below range collect
		(setf (payload (get-relative-node child :x x :y y)) (* x y))))

      (iterate child 3)
      child
      )))

(defclass geometry-payload ()
    ((geometry :initform nil)
     (fill-ratio :initform 0.0)))

(defclass tree-writer () 
  ((tree-base :initform (make-instance 'quadtree) :initarg target-tree)
   (max-lod :initform 4 :initarg max-lod))) 


(defstruct circle 
  (x 0.0 :type single-float)
  (y 0.0 :type single-float)
  (radius 0.0 :type single-float))

(utils:ex> 'make-circle)  

(with-package matrix
  (defstruct circle2
    (pos (make-vector '(0.0 0.0)) :type matrix)
    (radius 0.0 :type single-float)))

(utils:ex> 'make-circle2 'circle2)
(defmethod get-tree-writer ((tree-target quadtree) &key max-lod)
  (make-instance 'tree-writer :target-tree tree-target :max-lod max-lod))

(defun test-circle-quad (circle quad-x quad-y quad-lod)
  (let* ((quad-size (expt 2.0 quad-lod))
	 (quad-half-size (/ quad-size 2))
	 (quad-mid-x (+ quad-half-size quad-x))
	 (quad-mid-y (+ quad-half-size quad-y)))
    (with-slots ((circ-x x) (circ-y y) (circ-radius radius)) circle
      (and (< (abs (- circ-x quad-mid-x)) (+ quad-half-size circ-radius))
	   (< (abs (- circ-y quad-mid-y)) (+ quad-half-size circ-radius))))))
	   
(defmethod do-inscribe ((tree-target quadtree) (circle circle) lod x y)
  ())

(defmethod inscribe ((tree-target quadtree) (circle circle))
  ())


;(defun make-circle (&key radius diameter center upper-left-corner)
;  (let ((radius (cond (radius radius)
;		      (diameter (/2.0 diameter))
;		      (t 0.0)))
;	(center (cond (center center) 
;		      (upper-left-corner (lm:+ upper-left-corner (make-vector radius radius)))
;		      (t (make-vector 0.0 0.0)))))
 ;   (make-instance 'circle :radius radius :center center)))

(defun make-rectangle(&key size center upper-left-corner)
  ())

(defun simpler-example ()
  (let ((tree (make-instance 'quad-tree))
	(lod-interface (make-instance 'lod-interface :lod 4)))
    (setf (payload lod-interface :x 0 :y 0) (make-color :r 1.0 :g 1.0 :b 1.0 :a 1.0))))




(defun pdf-shaping-example ()
  ;Example of pdf-shaping
  (let ((tree (make-instance 'quadtree))
	(circle (make-circle :radius 0.5 :x 0 :y))
	(triangle (make-triangle :c1 (make-vector 0 0) :c2 (make-vector 1 0) (make-vector 0 1)))
	(rect (make-rectangle :mid-point (make-vector 3 3) :size (make-vector 1 1) :rotation (make-vector 1.0)))
	(tree-writer (get-tree-writer tree :default-max-lod 5)))

    (inscribe circle tree-writer)
    (inscribe triangle tree-writer :max-lod 4)
    (map (rcurry #'inscribe tree-writer) (triangulate rect))

    (let ((min-circle (make-circle :radius 0.5)))
      (do-times (n 10)
	(let* ((pt (random-point tree))
	       (max-radius-from-point (get-free-radius pt tree))
	       (circle (make-circle :radius (random-range 0.5 (max max-radius-from-point 1.0)) :center pt)))
	  (inscribe-convolved circle min-circle tree-writer))))

    (random-point tree)
    (let ((convolved-tree (convolve-tree tree circle)))
      (random-point convolved-tree))
    ))

(defun ascii-print-map (tree-base x-from x-to y-from y-to)
  (loop for x from x-from below x-to collect
       (loop for y from y-from below y-to collect
	    (payload (get-relative-node tree-base :x x :y y)))))
)

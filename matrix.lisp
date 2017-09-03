;;a more dynamic kind of vector
(re.load "cl-enumerate/cl-enumerate.lisp")
(defpackage :matrix (:use :cl-core :with-package) (:nicknames :m))
(in-package :matrix)

(utils:ex>

(defstruct matrix
  (shape (fixnum-vector) :type (cl:vector cl:fixnum) :read-only t)
  (data #() :type cl:vector :read-only t)
  )


(utils:ex> 'make-matrix 'matrix-shape 'matrix-data 'matrix-p)

(defun copy-matrix (mat)
  (make-matrix :shape (matrix-shape mat)
	       :data (alexandria:copy-array (matrix-data mat))))

(defun fixnum-vector(&rest values)
  (cl:make-array (cl:length values) :element-type 'cl:fixnum :initial-contents values))

(defun make-vector (values &key (row-vector t))
  (let ((value-length (cl:length values))
	(first-type (type-of (cl:first values))))
    (make-matrix :shape (if row-vector 
			    (fixnum-vector value-length) 
			    (fixnum-vector 1 value-length))
		 :data (cl:make-array 
			value-length 
			:element-type (if (cl:notevery (lambda (value) (eql (type-of value) first-type))
						   (cl:rest values))
					  t
					  first-type)
			:initial-contents values))))
;(1 2) -> 2
;(((1)) ((1))) -> (2 1 1)
(defun get-list-shape(lst)
  (if (cl:listp lst)
      (let ((l (cl:length lst))
	    (sublength (cl:mapcar #'get-list-shape lst)))
	(let ((fst (cl:first sublength)))
	  (loop for x in sublength do
	       (unless (equal x fst)
		 (error "Unbalanced list")))
	  
	  (cl:cons l fst)))
       
      nil))
(defun unfold-list (lst)
  (if (cl:listp lst)
      (let ((rest (cl:mapcar #'unfold-list lst)))
	(cl:apply #'cl:append rest))
      (cl:list lst)))

(defun list-values-to-array(lst)
  (let ((unfolded (unfold-list lst)))
    (cl:make-array (length unfolded) :initial-elements unfolded)))

(defun matrix (values)
  (let ((shape (cl:apply #'fixnum-vector (get-list-shape values)))
	(values (unfold-list values)))
    (make-matrix :shape shape :data (cl:make-array (cl:length values) :initial-contents values))))



(defun vec (&rest values)
  (make-vector values))

(defun ref (vec idx)
  (with-slots (data) vec
    (aref data idx)))

(defun (setf ref) (value vec idx)
  (with-slots (data) vec
    (setf (aref data idx) value)))

(defun matrix-apply (fcn &rest matrices)
  (let ((fst (matrix-shape (cl:first matrices)))
	(type (cl:type-of (matrix-data (cl:first matrices)))))
    (when (cl:notevery (lambda (m) (cl:equalp (matrix-shape m) fst)) (cl:rest matrices))
      (error "All matrices must have same size"))
    (when (cl:notevery (lambda (m) (cl:equalp (cl:type-of (matrix-data m)) type)) (cl:rest matrices))
      (setf type 'cl:vector))
    (make-matrix :shape fst
		 :data (cl:apply #'cl:map type fcn (cl:mapcar #'matrix-data matrices)))))

(defun get-2d-shape(mat)
  (let ((shape (matrix-shape mat)))
    (let ((len (cl:length shape)))
      (cond
	((cl:>= 0 len) (error "Invalid shape for 2d vector"))
	((cl:eq 2 len) shape)
	((cl:eq 1 len) (fixnum-vector (aref shape 0) 1))
	(t (if (cl:first (with-package enumerate 
			   (to-list 
			    (where 
			     (skip (walk-array shape) 2)
			   (lambda (x) (cl:not (eq x 1)))))))
	       (error "Invalid shape for 2d vector")
	       (fixnum-vector (aref shape 0) (aref shape 1))))))))


(defun get-row-shape (mat)
  (cl:with-slots (shape) mat
      (let ((non-row-shape 
	     (with-package enumerate
	       (to-array (skip 1 (walk-array shape)) 'fixnum))))
	(if (cl:equalp (fixnum-vector) non-row-shape)
	    (fixnum-vector 1)
	    non-row-shape))))

(defun enumerate-rows (mat)
  (cl:with-slots (shape) mat
    (cl:let* ((row-shape (get-row-shape mat))
	      (row-size (cl:reduce #'cl:+ row-shape))
	      (n-rows (cl:aref shape 0))
	      (mat-index 0))
      (lambda ()
	(if (cl:>= mat-index n-rows)
	    (enumerate:end)
	    (make-matrix 
	     :shape row-shape 
	     :data (cl:make-array row-size
				  :element-type (cl:array-element-type (matrix-data mat))
				  :displaced-to (matrix-data mat)
				  :displaced-index-offset (cl:* row-size  (utils:post-incf mat-index))))))
      )))


(defun get-dim-shape (mat dim)
  (cl:with-slots (shape data) mat
    
    (cl:values (aref data dim)
	    (with-package enumerate
	      (to-array (concat (take dim (walk-array shape))
				(take (cl:1- (cl:length shape)) 
				       (walk-array shape))) 'cl:fixnum)))))

(defun enumerate-columns(mat)
  (with-slots (data shape) mat
    (let ((rows (aref shape 0))
	  (cols (if (cl:> (cl:length shape) 1) (aref shape 1) 1))
	  (mat-index 0))

	(lambda ()
	  (if (cl:>= mat-index cols)
	      (enumerate:end)
	      (let ((super-idx (utils:post-incf mat-index)))
		(make-matrix 
		 :shape (fixnum-vector rows)
		 :data 
			(with-package enumerate
			  (to-array 
			   (select (lambda (idx) 
				     (aref data (cl:+ super-idx (cl:* idx cols))))
			   (range rows))))))))
	)))

(defun transpose(mat)
  (let ((shape (get-2d-shape mat)))
    (make-matrix 
     :shape (fixnum-vector 
	     (cl:aref shape 1) 
	     (cl:aref shape 0))
     :data (cl:apply #'cl:concatenate 'cl:vector 
		     (with-package enumerate 
		       (to-list (select (lambda (mat) (matrix-data mat)) 
					(enumerate-columns mat))))))))

(defun dot(b a)
  (with-package enumerate 
     (let ((a-cols (to-list (enumerate-columns a)))
	   (b-rows (to-list (enumerate-rows b))))
       (let ((data (make-vector
			  (loop 
			     for col in b-rows append
			       (loop 
				  for row in a-cols collect
				    (cl:reduce #'cl:+ (cl:map 'cl:vector #'cl:* 
							      (matrix-data col) 
							      (matrix-data row))))))))
	 (make-matrix :shape (fixnum-vector (cl:length a-cols) 
					    (cl:length b-rows))
		      :data (matrix-data data))
	 ))))
	 
(defun dot2(b a)
  (with-package enumerate 
     (let ((a-cols (enumerate-columns a)))

       (let ((out-data
	      (cl:apply #'cl:concatenate 
			'cl:vector 
			(to-list 
			 (select (lambda (col) 
				   (let ((rows (enumerate-rows b)))
		       	     (to-list (select (lambda (row)
							(cl:reduce #'cl:+ 
								   (cl:map 'cl:vector 
									   #'cl:* 
									   (matrix-data col) 
									   (matrix-data row))))
						      rows))))
				 a-cols)))))
       (make-matrix :shape (fixnum-vector (aref (matrix-shape b) 0) (aref (matrix-shape a) 1))
		    :data out-data)))))
	     
   
(defun test-enumerate-matrix-rows ()
  (let (
	(mat (make-matrix :shape (fixnum-vector 3 3) :data #(1 0 5
							     0 1 5
							     0 0 1)))
	)
    (dot (make-vector '(1 2 1))  mat)
    (print  (dot2 (make-vector '(3 4 5) :row-vector t) (make-vector '(5 6 7) :row-vector nil)))))

(defun lst-to-idx (mat indexlist)111
  (with-package enumerate
    (aggregate #'cl:+ 
	       (zip 
		(let ((dim-sum 1))
		  (lambda (x y)
		    (cl:+ (cl:* x (utils:post-set dim-sum (cl:* dim-sum y))))))
		(walk-list indexlist)
		(walk-array (matrix-shape mat))))))
  
(defun where (mat &rest index-value-sets)
  (let ((mat-copy (copy-matrix mat))
	(shape (matrix-shape mat)))
    (loop for vset in index-value-sets do
	 (setf (aref (matrix-data mat-copy) 
		     (lst-to-idx mat (cl:rest vset)))
	       (cl:first vset)))
    mat-copy))

(defun eye3x3()
  (make-matrix :shape (fixnum-vector 3 3) #(1.0 0.0 0.0 
					    0.0 1.0 0.0 
					    0.0 0.0 1.0)))
;(defun rot-2d(angle)
;  (let ((sin-ang (sin angle))
;	(cos-ang (cos angle)))
;    (let ((mat (eye3x3)))
;      (


(defmacro simple-varadic-defun (&rest names)
  (let ((forms (cl:first (cl:list (loop for (name fcn) in names collect
	     `(defun ,name (&rest args)
		(cl:apply #'matrix-apply #',fcn args)))))))
    
    (cl:push 'utils:ex> forms)
    
    forms))

(simple-varadic-defun (+ cl:+) (- cl:-) (* cl:*) (/ cl:/))

(defun scale (mat val)
  (matrix-apply (lambda (cell-value) (cl:* val cell-value)) mat))

)

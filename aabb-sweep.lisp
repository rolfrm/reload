(defun vcp (vec)
  (lm:* 1d0 vec))

(defmacro swap (setfable1 setfable2)
  `(psetf ,setfable1 ,setfable2 ,setfable2 ,setfable1))

(defun trunc2 (x)
  (if (or (float-nan-p x) (float-infinity-p x))
      x
      (/ (round (* x 1000.0)) 1000.0)))

(defun vec-eql(v1 v2)
  (let ((dim (lm:dimension v1)))
    
    (loop for x from 0 below dim do
	 (unless (eql (trunc2 (lm:elt v1 x)) (trunc2 (lm:elt v2 x)))
	   (return-from vec-eql nil)))
    t))

(defun line-intersection(a ad b bd)
  "solve for a + ad*t = b + bd*t
  same as (a - b)/( bd - ad) = t"
  (if (eql a b)
       (if (eql ad bd)
	   double-float-positive-infinity
	   0d0)
      (if (eql ad bd)
	  (*  double-float-positive-infinity (- b a))
	  (/  (- a b) (- bd ad)))))

(four-vector-element-function line-intersection line-vector-intersection)


(defun sweep-aabb-axis-vector(a1 a2 av b1 b2 bv)
  (progn
    ;;Since swapping might be needed. A copy of the vectors is needed.
    (setf a1 (vcp a1))
    (setf a2 (vcp a2))
    (setf b1 (vcp b1))
    (setf b2 (vcp b2))
    (setf av (vcp av))
    (setf bv (vcp bv))
    (loop for x from 0 below (lm:dimension a1) do
	 (let ((a (lm:elt a1 x))
	       (b (lm:elt b1 x)))

	   (when (< b a)
	     (swap (lm:elt a1 x) (lm:elt b1 x))
	     (swap (lm:elt a2 x) (lm:elt b2 x))
	     (swap (lm:elt av x) (lm:elt bv x))
	     )))
    (let ((from (line-vector-intersection a2 av b1 bv))
	  (to (line-vector-intersection a1 av b2 bv)))
      (loop for x from 0 below (lm:dimension a1) do
	  (let ((a (lm:elt from x))
	 	(b (lm:elt to x)))
	   
	    (when (> a b)
	      (swap (lm:elt from x) (lm:elt to x)))
	    )
	   (when (and (< (lm:elt from x) 0) (eql 0d0 (lm:elt to x)))
	     (setf (lm:elt to x) -0d0)))
      (values from to))))

(defun bounds-overlap (from to)
  "find the overlap between from and to. From < to.
if from == to it returns nil"
  (let ((dim (lm:dimension from)))
    (let ((globala double-float-negative-infinity)
	  (globalb double-float-positive-infinity))
      (loop for x from 0 below dim do
	   (let ((a (lm:elt from x))
		 (b (lm:elt to x)))
	     (setf globala (max a globala))
	     (setf globalb (min b globalb))))
      
      (< globala globalb))))


(defun test-bounds-overlap ()
  (let ((true-test-set '((1.0 2.0 3.0 4.0 5.0 6.0)
			 (1.0 1.0 1.0 2.0 2.0 2.0)
			 
			 ))
	(false-test-set '((1.0 2.0 3.0 2.0 3.0 4.0)
			  (1.0 1.0 1.0 1.0 1.0 1.0)
			 )))
    (loop for (a b c d e f) in true-test-set do
	 (unless (bounds-overlap (make-vector a b c) (make-vector d e f))
	   (print (format nil "Error bounds: (~a ~a ~a) overlap (~a ~a ~a) => false"
			  a b c d e f))))
    (loop for (a b c d e f) in false-test-set do
	 (when (bounds-overlap (make-vector a b c) (make-vector d e f))
	   (print (format nil "Error bounds: (~a ~a ~a) overlap (~a ~a ~a) => true"
			  a b c d e f))))
    ))


(defun calc-sweep-bounds(from to)
  "IF the bounds from and to overlap. return vector max of from and vector min of to
Also get the dimension in which from had max value"
  (if (bounds-overlap from to)
      (multiple-value-bind (time-to-col axis)
	  (vector-max-indexed from)
	(values time-to-col (vector-min to) axis))
      (values double-float-positive-infinity double-float-positive-infinity 1)
      ))

(defun aabb-sweep-collision-time (aabb1 offset1 speed1 aabb2 offset2 speed2)
  (let ((half-size1 (lm:* aabb1 0.5))
	(half-size2 (lm:* aabb2 0.5)))
	  
    (multiple-value-bind (critical-start critical-stop)
	(sweep-aabb-axis-vector (lm:- offset1 half-size1) (lm:+ offset1  half-size1) speed1
			    (lm:- offset2 half-size2) (lm:+ offset2 half-size2) speed2)
      (multiple-value-bind (coll-start coll-end collision-axis)
	  (calc-sweep-bounds critical-start critical-stop)
	
	
	(let ((collision-vector (lm:make-vector (lm:dimension aabb1))))
	  (setf (lm:elt collision-vector collision-axis) 
		(float-sign (- (lm:elt offset1 collision-axis) (lm:elt offset2 collision-axis))))
	  (if (< coll-start 0 )
	      (values coll-end coll-start collision-vector)
	      (values  coll-start coll-end collision-vector)))))))

(defun sweep-test (aabb1 aabb2)
  (multiple-value-bind (coll-start coll-end collision-vector)
      (aabb-sweep-collision-time (aabb-object-size aabb1)
				 (aabb-object-location aabb1)
				 (aabb-object-speed aabb1)
				 (aabb-object-size aabb2)
				 (aabb-object-location aabb2)
				 (aabb-object-speed aabb2))
    (values coll-start coll-end collision-vector)))

(defun vec-from-list (lst)
  (progn
    (apply #'make-vector lst)))

(defun create-aabb-lst (size loc vel)
  (make-aabb-object :size (vec-from-list size)
		    :location (vec-from-list loc)
		    :speed (vec-from-list vel)))

(defun test-sweep-test ()
  (let ((test-sets `( 
		     
		     ((1d0 1d0) (0d0 0d0) (0d0 0d0)
		     (1d0 1d0) (2d0 2d0) (-1d0 -1d0)
		     1d0 3d0)

		     ((1d0 1d0) (0d0 0d0) (0d0 0d0)
		     (1d0 1d0) (1d0 1d0) (-1d0 -1d0)
		     0d0 2d0)

		     ((1d0 1d0) (0d0 0d0) (0d0 0d0)
		     (1d0 1d0) (1d0 1d0) (1d0 1d0)
		      ,-0d0 -2d0)

		     ((1d0 1d0) (0d0 0d0) (-1d0 -1d0)
		     (1d0 1d0) (1d0 1d0) (0d0 0d0)
		      ,-0d0 -2d0)


		     ((1d0 1d0) (0d0 0d0) (0d0 0d0)
		     (1d0 1d0) (2d0 2d0) (0d0 0d0)
		     ,double-float-positive-infinity ,double-float-positive-infinity)
		     )))
    (loop for (a-size a-loc a-vel 
		    b-size b-loc b-vel
		    collision-time collision-end) in test-sets
	 for x from 0 below (length test-sets) do
	 (print x)
	 (let ((a (create-aabb-lst a-size a-loc a-vel))
	       (b (create-aabb-lst b-size b-loc b-vel)))
	   (multiple-value-bind (col-start col-end vec) 
	       (sweep-test a b)

	     (unless (and (eql col-end collision-end) (eql col-start collision-time))
	       (print 
		(format nil "ERROR 1: collision time mis-calculation: got ~a ~a expected ~a ~a" col-start col-end
			collision-time collision-end))))
	   (multiple-value-bind (col-start col-end vec) 
	       (sweep-test b a)

	     (unless (and (eql col-end collision-end) (eql col-start collision-time))
	       (print 
		(format nil "ERROR 2: collision time mis-calculation: got ~a ~a expected ~a ~a"  col-start col-end
			collision-time collision-end))))))))

	     
(defun test-lvi (a av b bv)
  (lm:elt (line-vector-intersection (make-vector a) (make-vector av)
			    (make-vector b) (make-vector bv)) 0))


(defun test-line-vector-intersect ()
  (let ((inf double-float-positive-infinity))
    (let ((test-set
	   (list (list 0 0 0 0 0d0)
		 (list 0 1 1 0 1d0)
		 (list 1 0 0 1 1d0)
		 (list 0 0 0 1 0d0)
		 (list 1 0 0 -1 -1d0)
		 (list 1 0 0 0 inf)
		 (list 0 0 1 0 inf)
		 (list 0 1 1 -1 0.5d0)
		 (list -2 1 2 -1 2d0)
		 (list 1 1 1 1 0d0)
		 (list 1 1 0 1 inf)
		 (list 0 -1 1 1 -0.5d0)
		 ))
	  (ok t))
      (loop for (a av b bv result) in test-set do
	   (unless (equal (test-lvi a av b bv) result)
	     ;try both ways, results should be symmetrical
	     (print "ERROR:")
	     (print (format nil "got: ~a => ~a" (list a av b bv) (test-lvi a av b bv)))	
	     (print (format nil "exp: ~a => ~a" (list a av b bv) result)))
	   (unless (equal (test-lvi b bv a av) result)
	     (print "ERROR:")
	     (print (format nil "got: ~a => ~a" (list b bv a av) (test-lvi b bv a av)))	
	     (print (format nil "exp: ~a => ~a" (list b bv a av) result)))))))

(defun test-sweep-ab-axis-vector ()
  (let ((a1 (make-vector -0.179 -0.119 0.000))
	(a2 (make-vector (- 0.01 0.021) 0.081 0.200))
	(av (make-vector 0.045 0.100 0.000))
	(b1 (make-vector  0.021 -0.119 0.000 ))
	(b2 (make-vector 0.221 0.081 0.200))
	(bv (make-vector  0.045 -0.005 0.000)))
    (sweep-aabb-axis-vector a1 a2 av b1 b2 bv)))
(defvar d-inf double-float-positive-infinity)
(defun sweep-aabb-axis-vector-test ()
  (let ((test-sets `(
		      ((0.0 0.0) (1.0 1.0) (1.0 1.0)
		       (2.0 2.0) (3.0 3.0) (-1.0 -1.0)
		      (0.5 0.5) (1.5 1.5))
		  
		      ((0.0 2.0) (1.0 3.0) (1.0 -1.0)
		       (2.0 0.0) (3.0 1.0) (-1.0 1.0)
		       (0.5 0.5) (1.5 1.5))

		      ((0.0 0.0) (1.0 0.0) (0.0 0.0)
		       (1.0 1.0) (2.0 1.0) (0.0 0.0)
		       (,d-inf ,d-inf) (,d-inf ,d-inf))

		      ((0.0 0.0) (1.0 1.0) (0.0 0.0)
		       (1.0 1.0) (2.0 2.0) (1.0 1.0)
		       (-2d0 -2d0) (-0d0 -0d0))
		     
		     ((0.5 0.5) (1.5 1.5) (0.0 0.1)
		      (1.5 1.5) (2.5 2.5) (0.0 -0.1)
		      (,d-inf 0.0) (,d-inf 10.0))

		     )))
    
    (loop for (a1 a2 av b1 b2 bv from to) in test-sets 
	 for x from 0 to (length test-sets) do
	 (print x)
	 (multiple-value-bind (calc calc-to)

	     (sweep-aabb-axis-vector (vec-from-list a1) (vec-from-list a2) (vec-from-list av)
				     (vec-from-list b1) (vec-from-list b2) (vec-from-list bv))
	   (unless (and (vec-eql calc-to (vec-from-list to)) 
			(vec-eql calc (vec-from-list from)))
	     (print "Error 1:")
	     (print calc-to)
	     (print (vec-from-list to))
	     (print calc) 
	     (print (vec-from-list from))

	     ))
	 (multiple-value-bind (calc calc-to)
	     
	     (sweep-aabb-axis-vector (vec-from-list b1) (vec-from-list b2) (vec-from-list bv)
				     (vec-from-list a1) (vec-from-list a2) (vec-from-list av))
	   (unless (and (vec-eql calc-to (vec-from-list to)) 
			(vec-eql calc (vec-from-list from)))
	     (print "Error 2:")
	     
	     (print calc-to)
	     (print (vec-from-list to))
	     (print calc) 
	     (print (vec-from-list from))
	     )))))

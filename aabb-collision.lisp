(defun aabb-aabb-intersection (aabb1 offset1 aabb2 offset2)
  "Calculates the intersection vector between the triangle and the aabb"
  (let ((distance-vector (lm:- offset1 offset2))
	(combined-size (lm:* (lm:+  aabb1 aabb2) 0.5)))
    (let ((overlap (lm:- combined-size (vector-abs distance-vector))))
      (values overlap distance-vector))))

(defun simple-resolve-collision(aabb1 offset1 aabb2 offset2)
  (multiple-value-bind(overlap difference)
      (aabb-aabb-intersection aabb1 offset1 aabb2 offset2)
    (let ((moveout 
	   (min-or-zero 
	    (lm:* (vector-sign difference) 
		  (vector-only-positive overlap)))))
      (values (lm:* 0.5000001 moveout) 
	      (lm:* -0.5000001 moveout)
	      ))))

(defun aabb-resolve-negative (aabb1 aabb2)
  (multiple-value-bind (moveout1 moveout2)
      (simple-resolve-collision 
       (aabb-object-size aabb1)
       (aabb-object-location aabb1)
       (aabb-object-size aabb2)
       (aabb-object-location aabb2))
    (setf (aabb-object-location aabb1) (lm:+ moveout1 (aabb-object-location aabb1)))
    (setf (aabb-object-location aabb2) (lm:+ moveout2 (aabb-object-location aabb2)))
    (setf (aabb-object-is-dirty aabb1) t)
    (setf (aabb-object-is-dirty aabb2) t)
    ))

(defun does-collide(aabb1 offset1 aabb2 offset2)
  "Returns true if the rectangles overlaps"
  (multiple-value-bind (overlap distance-vector)
      (aabb-aabb-intersection aabb1 offset1 aabb2 offset2)
    (< 0 (vector-min overlap))))

(defun aabb-does-collide (aabb1 aabb2)
  (does-collide
   (aabb-object-size aabb1)
   (aabb-object-location aabb1)
   (aabb-object-size aabb2)
   (aabb-object-location aabb2)))
		

(defun uniform-vector (from to)
  (random-vector (make-vector from from from)
		 (make-vector to to to)))

;;;TESTS 

(defun test-aabb-resolve ()
  (let ((size (make-vector 1d0 1d0 1d0))
	(v1 (uniform-vector 0d0 1.1d0))
	(v2 (uniform-vector 0d0 1.1d0)))
    (let ((aabb1 (make-aabb-object :size size :location v1))
	  (aabb2 (make-aabb-object :size size :location v2)))
	  (if (aabb-does-collide aabb1 aabb2)
	      (progn
		(aabb-resolve-negative aabb1 aabb2)
		(if (aabb-does-collide aabb1 aabb2)
		    (error (list "AABBS shouldent be colliding" aabb1 aabb2))
		    (print "success")))
	      (print "no initial collision")))))


(defun test-colliding-aabbs (times)
  (let ((size (make-vector 1d0 1d0 1d0)))
    (dotimes (n times)
      (let ((v1 (uniform-vector 0d0 1.0))
	    (v2 (uniform-vector 0d0 1.0)))
	(let ((collides 
	       (does-collide size v1 size v2)))
	  (unless collides
	    (print v1)
	    (print v2)
	    (print size)
	    (error (list 
		    "doesnt collide!" size v1 size v2)
		   )))))))

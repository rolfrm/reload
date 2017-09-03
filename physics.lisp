(declaim (optimize (speed 0) (safety 3) (space 0) (debug 3)))

(re.load "double-math.lisp")
(re.load "physics-vector.lisp")
(re.load "aabb-math.lisp")
(re.load "aabb-collision.lisp")
(re.load "aabb-sweep.lisp")
(re.load "collision-table.lisp")

(defclass physics-context ()
  ((physics-objects :initform nil :initarg :objects)
   (current-time :initform 0d0)
   (collision-table :initform (make-instance 'collision-table))))
 
(defun truncatek (v)
  (* (floor (* v 10000)) 0.0001))


(defmethod physics-iterate ((context physics-context) delta-time)
  ;(proclaim '(optimize debug))
  (with-slots(collision-table physics-objects current-time) context
    (progn
      (collision-table-update2 collision-table physics-objects current-time))
      ;(print (mapcar #'collision-table-entry-time (slot-value collision-table 'collision-table)))
      (let* ((first-table (first (slot-value collision-table 'collision-table))) 
	     (first-time
		   (if first-table 
		       (collision-table-entry-time first-table)
		       10000000)))
	(let ((actual-dt (min (- first-time current-time) delta-time)))
	  (if (< first-time current-time)
	      (error  "collision") ;;This should never happen during normal use
	      (progn
		(incf current-time actual-dt)
		(loop for p-obj in physics-objects do
		     (setf (aabb-object-location p-obj) 
			   (lm:+ (aabb-object-location p-obj)
				 (lm:* (aabb-object-speed p-obj) (* 0.9999 actual-dt)))))
		
		(when (< actual-dt delta-time)
		  (let ((collider-a (collision-table-entry-a first-table))
			(collision-vector (collision-table-entry-normal-vector first-table))
			(collider-b (collision-table-entry-b first-table)))
		    (collision-response collider-a collider-b (lm:normalise collision-vector))
		     )
		  (physics-iterate context (- delta-time actual-dt))
		  )
	
		(values (- delta-time actual-dt) collision-table)
		))))))

(defmethod remove-physics-object ((context physics-context) aabb-object)
  (with-slots (physics-objects collision-table) context
    (setf physics-objects (remove aabb-object physics-objects))
    (setf collision-table (remove-if 
			   (lambda (x) 
			     (or (eql (collision-table-entry-a x) aabb-object) 
				 (eql (collision-table-entry-b x) aabb-object))) 
			   collision-table))))
     
	  
(defun collision-response (aabb1 aabb2 normal-vector)
  "Calculates the change in speed due the collision"
  (let ((mass1 (aabb-object-mass aabb1)) 
	(mass2 (aabb-object-mass aabb2))
        (loc1 (aabb-object-location aabb1))
	(loc2 (aabb-object-location aabb2)))
    (let ((velocity1 (aabb-object-speed aabb1))
	  (velocity2 (aabb-object-speed aabb2)))
      (let ((diff-velocity (lm:- velocity1 velocity2)))
	(let ((impulse (/ (lm:dot-product 
			     (lm:* -1d0 diff-velocity)
			     normal-vector) 
			    (+ (/ 1d0 mass1) 
			       (/ 1d0 mass2)))))
	  (setf (aabb-object-speed aabb1) 
		(lm:+ velocity1 (lm:* normal-vector (/ impulse mass1))))  
	  (setf (aabb-object-speed aabb2) 
		(lm:- velocity2 (lm:* normal-vector (/ impulse mass2))))
	  (setf (aabb-object-is-dirty aabb1) t)
	  (setf (aabb-object-is-dirty aabb2) t))))))

(defmethod is-update-needed ((context physics-context))
  (with-slots (physics-objects) context
    (some #'aabb-object-is-dirty physics-objects)))

				  
(defun check-if-collides (aabb1 aabb2)
  (does-collide (aabb-object-size aabb1)
		(aabb-object-location aabb1)
		(aabb-object-size aabb2)
		(aabb-object-location aabb2)))


(defun test-physics-1 ()
  (let* ((objects 
	 (list
	  (make-aabb-object :size (make-vector 1 1 1)
			    :location (make-vector 0 0 0)
			    :speed (make-vector 0.1 0.1 0.1))
	  (make-aabb-object :size (make-vector 1 1 1)
			    :location (make-vector 2 2 2)
			    :speed (make-vector -0.1 -0.1 -0.1))
	  (make-aabb-object :size (make-vector 1 1 1)
			    :location (make-vector 4 4 4)
			    :speed (make-vector -0.2 -0.2 -0.2))
	  (make-aabb-object :size (make-vector 1 1 1)
			    :location (make-vector 5 5 5)
			    :speed (make-vector 0.2 0.2 0.2))

	  ))
	(col-table (make-instance 'physics-context :objects objects)))
    (loop for x from 0 to 20 do
	 (physics-iterate col-table 1.0))))

(defun make-physics-context()
  (let ((objects 
	 (list (make-aabb-object :size (make-vector 1 1 1) :location (make-vector 0 0 0) :speed (make-vector 0.0 0.01 0.01))
	       (make-aabb-object :size (make-vector 1 1 1) :location (make-vector 2 0 0) :speed (make-vector -0.2 -0.01 -0.01)))))
  (let ((phys-ctx (make-instance 'physics-context :objects objects)))
    phys-ctx)))


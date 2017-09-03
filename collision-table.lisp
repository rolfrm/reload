

(defstruct collision-table-entry
  (time 0d0 :type double-float)
  (time-end 0d0 :type double-float)
  (a (make-aabb-object) :type aabb-object)
  (b (make-aabb-object) :type aabb-object)
  (normal-vector (lm:make-vector 3) :type lm:vector))

(defclass collision-table()
  ((collision-table :initform nil)))

(defmethod remove-aabb ((collision-table collision-table) aabb)
  (with-slots (collision-table-entries) collision-table
    ())) ;;to do

(defmethod collision-table-update2 ((collision-table collision-table) physics-objects current-time)
  (with-slots (collision-table) collision-table
    (let ((dirty-objects (remove-if-not #'aabb-object-is-dirty physics-objects)))
      (setf collision-table 
	    (remove-if 
	     (lambda (x) 
	       (or (find (collision-table-entry-a x) dirty-objects) 
		   (find (collision-table-entry-b x) dirty-objects))) 
	    collision-table))
      (let ((new-col-entries nil))
	(loop for dirty-aabb in dirty-objects do
	     (progn
	       (setf (aabb-object-is-dirty dirty-aabb) nil)
	       (loop for other-aabb in  physics-objects do
		    (when (not (eq other-aabb dirty-aabb))
		      (multiple-value-bind (time coll-end col-vector)
			  (sweep-test dirty-aabb other-aabb)
			(when (and (< (float-sign coll-end) 0d0) (< (float-sign time) 0d0))
			  (setf time double-float-positive-infinity)
			  (setf coll-end double-float-positive-infinity))
			(when (not (float-infinity-p time))
			  (push (make-collision-table-entry :time (+ current-time time) :time-end (+ current-time coll-end) :a dirty-aabb 
							    :b other-aabb :normal-vector col-vector) new-col-entries)))))))
	(setf collision-table
	      (merge 'list collision-table 
		     (sort new-col-entries #'< :key #'collision-table-entry-time) 
		     #'< :key #'collision-table-entry-time))
	))))

(defun test-collision-table ()
  (let ((objects 
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
	(col-table (make-instance 'collision-table)))
    (collision-table-update col-table objects)
    (print (slot-value col-table 'collision-table))))

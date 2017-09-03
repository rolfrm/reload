(defmethod serialize (any.object &optional (htb '()))
  (declare (ignore htb))
  any.object)

(defmethod serialize ((obj standard-object) &optional (htb (make-hash-table)))
  (if (gethash obj htb)
      obj
      (let*((slot-names (class-slot-list (class-of obj)))
	    (slot-values (mapcar (curry #'get-value obj) slot-names)))
	(setf (gethash obj htb) t)
	(append (list (class-of obj)) 
		(mapcar #'list
			slot-names
			(mapcar (rcurry #'serialize htb) 
				slot-values))))))

(defun object-is-class (obj)
  (symbol-is-class (type-of obj)))

(defun symbol-is-class (symb)
  (subtypep symb 'standard-object))

(defun deserialize (serialized-obj obj)
  "Maps serialized obj to obj"
  (if (listp serialized-obj)
      (let ((lst-first (first serialized-obj)))
	(if (and (symbolp lst-first) (symbol-is-class lst-first))
	    (loop for x in (rest serialized-obj) do
		 (let ((slot (first x)) (value (second x)))
		   (set-value obj slot (deserialize value))))
	    serialized-obj)
	    ))
      serialized-obj)
	
  


(defun serialize-graph (obj &optional (htb (make-hash-table)))
  (serialize obj htb))
      

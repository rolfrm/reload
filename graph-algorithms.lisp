(defmethod huestic-cost ((vert-a vertex) (vert-b vertex))
  100.0)

(defun a-star(start-vertex end-vertex)
  (let ((closed-set (list start-vertex))
	(stacks (list 
		 (list 0.0 
		       (huestic-cost start-vertex end-vertex) 
		       (list start-vertex))
		 )))
    (loop while stacks do
	 (let ((next (pop stacks)))
	   (let ((traveled (pop next))
		 (heustic (pop next))
		 (vertexes (pop next)))
	     (let ((vert (first vertexes)))
	       (let ((connected (remove-if (lambda (vert) (find vert closed-set))
					   (get-connected-verts vert))))
		 (loop for conn in connected do
		      (if (eq conn end-vertex)
			  (return-from a-star (cons conn vertexes))
			  (let ((d (huestic-cost conn end-vertex))
				(d2 (edge-cost (get-edge vert conn))))
			    (push conn closed-set)
			    (setf stacks 
				  (utils:sorted-insert stacks 
						 (list (+ d2 traveled) 
						       d 
						       (cons conn vertexes))
						 :key (lambda (cell) (+ (first cell) 
									(second cell)))
						 :test #'<
						 ))
			    )))))))
	 )))

(defun daijkstra-search (vertex predicate &optional (max-distance double-float-positive-infinity))
  (let ((closed-set (list vertex))
	(stacks (list 
		 (list 0.0 (list vertex))
		 )))
    (loop while stacks do
	 (let ((next (pop stacks)))
	   (let ((traveled (pop next))
		 (vertexes (pop next)))
	     (let ((vert (first vertexes)))
	       (let ((connected (remove-if (rcurry #'find closed-set)
					   (get-connected-verts vert))))
		 (loop for conn in connected do
		      (if (funcall predicate conn)
			  (return-from daijkstra-search (cons conn vertexes))
			  (let ((d(vertex-distance vert conn)))
			    (push conn closed-set)
			    (setf stacks 
				  (utils:sorted-insert stacks 
						 (list d (cons conn vertexes))
						 :key #'first
						 :test #'<
						 ))
			    )))))))
	 )))

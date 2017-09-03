(defclass graph-game-engine-renderer(graph-renderer)
  ((render-view :initarg :render-view) 
   (graph :initarg :graph)))

(defmethod do-render-graph ((graph-editor graph-game-engine-renderer) win)
  (with-slots ((rview render-view) graph ) graph-editor
    (with-slots (vertexes) graph
      (loop for vert in vertexes do
	   (with-slots (occupiers) vert
	     (loop for occ in occupiers do
		  (do-update-graph-occupier occ)))))
    
    (setf (render-view-window-size rview) (matrix:make-vector (get-window-size win)))
    (render-graph-game-engine win graph rview)
    ))



;;Rendering of graph / graphics
(defun render-graph-game-engine(win graph render-view)
  (flet ((unif (name &rest args)
	   (apply #'set-uniform gpu-prog name args)))
    (with-slots(edges vertexes) graph
      (unif "cameraZoom" (render-view-camera-zoom render-view))
      (with-vector-reader v 
	(render-view-camera-position render-view)
	(unif "cameraPos" (v 0) (v 1)))
      
      (unif "uv_start" 0.0 0.0)
      (unif "uv_stop" 0.2 0.3)
      
      (bind-array vbo win 0)
      
      (loop for (a-vert b-vert) in 
	   (mapcar (lambda (edge) (get-vertexes graph edge)) edges) 
	 do
	   (with-vector-reader 
	       a 
	     (vertex-location a-vert)
	     (with-vector-reader
		 b (matrix:- (vertex-location b-vert)
			     (vertex-location a-vert))
		 (unif "offset" (a 0) (a 1))
		 (unif "size" (b 0) (b 1))
		 (unif "color" 1.0 1.0 1.0 0.3)
		 (bind-program gpu-prog win)
		 (gl:draw-arrays :lines 0 2)))
	   )
      (bind-array square-vbo win 0)
      (let ((zoom (render-view-camera-zoom render-view)))
	
	(unif "color" 0.0 1.0 0.0 0.4)
	(let ((ctx (make-instance 'graphical-context :shader gpu-prog :window win)))
	  
	  (loop for vert in (slot-value graph 'vertexes) do
	       (with-vector-reader 
		   loc (vertex-location vert)
		   
		   (unif "color" 0.0 0.0 0.0 0.5)
		   (unif "size" 
			 (/ 0.01 zoom) 
			 (/ 0.01 zoom))
		   (unif "offset" (loc 0) (loc 1))
		   (bind-program gpu-prog win)
		   ;(gl:draw-arrays :quads 0 4)
		   (let ((occ (utils:unfold-slot vert occupiers)))
		     (loop for oc in occ do
			  (with-slots (graphical-model offset) oc
			    (render-graphical-model 
			     graphical-model 
			     ctx 
			     (matrix:+ (vertex-location vert) 
				       offset)
			     (matrix:make-vector '(1.0 1.0 1.0)))

			    )))

		   ))
	  )))))

(defun move-to (obj graph window-pos render-view)
  (let* ((screen-pos (window-pos-to-screen-pos window-pos render-view))
	 (vertex (get-nearest-screen-vertex graph screen-pos 1.0 render-view)))
    (setf (slot-value obj 'target-vertex) vertex)))

(defun load-game-engine (graph)
  (let* (
	 (event-handler (load-ui-events))
	 (renderer (make-instance 'graph-game-engine-renderer :render-view (make-render-view :camera-zoom 0.3)
				  :graph graph)))
    (set-ui-event-handler event-handler)
    (with-slots (render-view) renderer

      (with-slots (mouse-scroll key-event mouse-move mouse-button) event-handler
	(reactive:subscribe mouse-scroll
			    (lambda (x y) 
			      (let 
				  ((delta (if (< 0 y) 1.2 0.8)))
				(setf (render-view-camera-zoom render-view)
				      (* (render-view-camera-zoom render-view) delta)))))
            (let* (
	     (key-map (make-hash-table))
	     (emitters (load-mouse-events mouse-move mouse-button 1.0 'left))
	     (left-mouse-drag (first emitters))
	     (left-mouse-click (second emitters))
	     (left-mouse-up (third emitters))
	     (right-emitters (load-mouse-events mouse-move mouse-button 1.0 'right))
	     (right-mouse-click (second right-emitters))
	     (right-mouse-drag (first right-emitters))
	     (last-pos (matrix:make-vector '(0.0 0.0)))
	     (pos (matrix:make-vector '(0.0 0.0)))
	     (status-changed nil)
	     )
	(flet ((is-down(key)
		 (or (eq (gethash key key-map) 'repeat) 
		     (eq (gethash key key-map) 'down))))
	  (reactive:subscribe left-mouse-click 
			      (lambda (pos) 
				(mapcar (lambda (player) 
					  (move-to player graph pos render-view))
					player)))
	  (reactive:subscribe (key-down 'escape key-event) (lambda () (load-graph-editor graph)))

	  (reactive:subscribe (key-down 'd key-event) 
			      (lambda () (mapcar 
					  (lambda (player)
					    (setf (slot-value player 'target-vertex)
					      	  (first
						   (daijkstra-search (occupant-vertex player) 
								     (lambda (vertex)
								       (slot-value vertex 'occupiers)))))
					    render-view)
				      player)))

	  (reactive:subscribe right-mouse-drag 
			      (lambda (motion)
				
				(setf (render-view-camera-position render-view)
				      (matrix:+ (matrix:scale 
						 (matrix:make-vector 
						  (list (matref motion 0) 
							(- (matref motion 1)) 
							0.0))
						 (* 0.01 (/ 1.0 (render-view-camera-zoom render-view))))
						(render-view-camera-position render-view)))))

	  ))))
    (set-current-graph-renderer renderer)))


(defclass graph-editor-renderer(graph-renderer)
  ((render-view :initarg :render-view) 
   (graph :initarg :graph)
   (selected-vertexes :initform nil)))

(defmethod do-render-graph ((graph-editor graph-editor-renderer) win)
  (with-slots ((rview render-view) graph (sel-vert selected-vertexes)) graph-editor
    (setf (render-view-window-size rview) (matrix:make-vector (get-window-size win)))
    (render-graph-editor win graph rview sel-vert)
   ))

;;Rendering of graph / graphics
(defun render-graph-editor(win graph render-view selected-vertexes)
  (flet ((unif (name &rest args)
	   (apply #'set-uniform gpu-prog name args)))
    (with-slots(edges vertexes) graph
      (unif "cameraZoom" (render-view-camera-zoom render-view))
      (with-vector-reader v 
	(render-view-camera-position render-view)
	(unif "cameraPos" (v 0) (v 1)))
      (bind-array vbo win 0)
      (bind-array vbo win 1)
      (unif "uv_start" 0.0 0.0)
      (unif "uv_stop" 1.0 1.0)
      (loop for (a-vert b-vert) in 
	   (mapcar (lambda (edge) (get-vertexes graph edge)) edges) 
	 do
	   (with-vector-reader 
	       a 
	     (vertex-location a-vert)
	     (with-vector-reader
		 b (matrix:- (vertex-location b-vert)
			     (vertex-location a-vert))
		 (unif "sampler" 0)
		 (unif "offset" (a 0) (a 1))
		 (unif "size" (b 0) (b 1))
		 (unif "color" 1.0 1.0 1.0 1.0)
		 
		 (bind-program gpu-prog win)
		 (gl:draw-arrays :lines 0 2)))
	   )
      (bind-array square-vbo win 0)
      (let ((zoom (render-view-camera-zoom render-view)))
	(unif "size" 
	      (/ 0.03 zoom) 
	      (/ 0.03 zoom)))
      (unif "color" 0.0 1.0 0.0 0.4)

      (loop for vert in (slot-value graph 'vertexes) do
	   (with-vector-reader 
	       loc (vertex-location vert)
	       (if (find vert selected-vertexes)
		   (unif "color" 0.0 1.0 0.0 0.4)
		   (unif "color" 1.0 1.0 1.0 0.4))
	       
	       (unif "offset" (loc 0) (loc 1))
	       (bind-program gpu-prog win)
	       (gl:draw-arrays :quads 0 4)))
      )))

(defun load-graph-editor (graph)
  (let* ((event-handler (load-ui-events))
	 (render-view (make-render-view :camera-zoom 0.3))
	 (renderer (make-instance 'graph-editor-renderer :render-view render-view :graph graph)))
    (with-slots (selected-vertexes render-view) renderer
      (set-ui-event-handler event-handler)
					;UI / graphics
      (with-slots (mouse-scroll) event-handler
	(reactive:subscribe mouse-scroll
			    (lambda (x y) 
			      (let 
				  ((delta (if (< 0 y) 1.2 0.8)))
				(setf (render-view-camera-zoom render-view)
				      (* (render-view-camera-zoom render-view) delta))))))
    
					;graph editor mouse bindings.
      (with-slots (mouse-move mouse-button mouse-scroll key-event ) event-handler

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

	  (reactive:subscribe key-event (lambda (key action) (setf (gethash key key-map) action)))
	  (reactive:subscribe mouse-move
			      (lambda (mouse-position) 
				(progn (setf last-pos pos) 
				       (setf pos (window-pos-to-screen-pos mouse-position render-view)))))

	  (reactive:subscribe 
	   left-mouse-drag 
	   (lambda (motion)
	     (loop for vert in selected-vertexes do
		  (with-slots (location) vert
		    (let ((pos (screen-to-world-project pos location render-view))
			  (lpos (screen-to-world-project last-pos location render-view)))
		      (setf location (matrix:+ location (matrix:- pos lpos))))))))

	  (flet ((on-left(pos is-click)
		   (let* ((screen-pos (window-pos-to-screen-pos pos render-view))
			  (vertex (get-nearest-screen-vertex grph screen-pos 1.0 render-view))
			  (in-selected (find vertex selected-vertexes)))
		     (if (is-down 'shift)
			 (if in-selected
			     (when is-click
			       (delete vertex selected-vertexes))
			     (unless is-click
			       (push vertex selected-vertexes)))
			 (if in-selected
			     (when is-click
			       (when (not status-changed)
				 (setf selected-vertexes (list vertex)))
			       (setf status-changed nil))
			     (unless is-click
			       (setf status-changed t)
			       (setf selected-vertexes (list vertex))))))))
	    
	    (reactive:subscribe left-mouse-click (rcurry #'on-left t))
	    (reactive:subscribe left-mouse-up (rcurry #'on-left nil))
	    )
	  (reactive:subscribe (key-down 'e key-event) (lambda () (split-selected-edges selected-vertexes)))
	  (reactive:subscribe (key-down 'c key-event) (lambda () (connect-selected-vertexes selected-vertexes)))
	  (reactive:subscribe (key-down 'escape key-event) (lambda () (load-game-engine graph)))
	  (reactive:subscribe (key-down 'd key-event) 
			      (lambda ()
				(when (>= (length selected-vertexes) 2)
				  (setf selected-vertexes
					(a-star (first selected-vertexes) 
						(second selected-vertexes)))
				  )))

	  (reactive:subscribe 
	   right-mouse-click 
	   (lambda (pos)
	     (when (is-down 'left-control)
	       (let* ((screen-pos (window-pos-to-screen-pos pos render-view ))
		      (world-pos (screen-to-world-project screen-pos (matrix:make-vector (list 0.0 0.0 0.0)) 
							  render-view))) 
		 (make-vertex grph :location  world-pos)))))
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

	  )))

    (set-current-graph-renderer renderer)
    )))

					; global graph / main game structure
					; UI only part of editor.
					;render graphics / UI

					;UI / Editor
(defun split-selected-edges (selected-vertexes)
  (let ((edges (mapcar #'vertex-edges selected-vertexes)))
    (with-package enumerate
      (let* ((all-elements 
	      (to-list (select-many-list (lambda (x) x) (walk-list edges))))
	     (uniques (remove-duplicates all-elements)))
	(loop for elem in uniques do
	     (setf all-elements (remove elem all-elements :count 1) ))
	(loop for elem in all-elements do
	     (split-edge elem grph))
	))))

					;UI / Editor
(defun connect-selected-vertexes(selected-vertexes)
  (let ((verts selected-vertexes))
    (loop for v1 in verts do
	 (loop for v2 in verts do
	      (if (eq v1 v2)
		  nil
		  (if (find v2 (get-connected-verts v1))
		      nil
		      (connect-vertexes grph v1 v2)))
	      ))
    ))

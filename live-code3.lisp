
(defvar frag-obj (make-instance 'gpu-program-object :kind :fragment))
(defvar vert-obj (make-instance 'gpu-program-object :kind :vertex))

(set-shader-code frag-obj "
uniform vec4 color;
varying vec2 uv;
void main(){
  
  gl_FragColor = color;
}")
(set-shader-code vert-obj "
attribute vec2 pos;
uniform vec2 offset;
uniform vec2 cameraPos;
uniform vec2 cameraZoom;
uniform vec2 size;
void main(){
  gl_Position = vec4((pos * size + offset - cameraPos)*cameraZoom,0.0,1.1);

}
")


(defvar gpu-prog (make-instance 'gpu-program :vertex vert-obj :fragment frag-obj))
(defvar vbo (make-instance 'gpu-array))

(defvar itasd 0.5)
(defparameter dit 0.01)
(set-vertexes vbo '(0.0 0.0 
		    1.0 0.0 
		    1.0 1.0 
		    0.0 1.0))

(defvar line-vbo (make-instance 'gpu-array))
(set-vertexes line-vbo '(0.0 0.0 1.0 1.0))


(defun create-sea-line (max-depth falloff &optional (lod-scale 1.0))
  (if (< max-depth 0)
      '(0.0 0.0 0.0 0.0)
      (let ((rest-line (create-sea-line (1- max-depth) falloff (* lod-scale falloff)))
	    (scale lod-scale))

	(with-package enumerate
	  (to-list (select-many-list
		    (let ((phase (utils:random-range 0 6.4)))
		      (lambda (y) (list  (+ y (* scale (sin (incf phase 0.2))))
					 (+ y (* scale (sin (incf phase 0.2)))))))
		    (walk-list rest-line)))))))

(defparameter sea (create-sea-line 3 0.95)) 
(defun /1 (x) (/ 1.0 x))

(defun _mean(a b)
  (/ (+ a b) 2))

(defun render-sea (sea win)
  (let* ((len (length sea))
	 (min (apply #'min sea))
	 (max (apply #'max sea))
	 (range (- max min))
	 (cam-height (* 0.5 range))
	 (cam-width len)
	 (center-x (/ len 2.0))
	 (center-y (_mean min max))
	 )

    (set-uniform gpu-prog "cameraZoom" (* 2.0 (/1 cam-width)) (/1  cam-height))

    (set-uniform gpu-prog "cameraPos" center-x center-y)
    (bind-array vbo win 0)
    
    (loop for x in sea 
       for idx from 0 to 10000 do
	 (progn
	   (set-uniform gpu-prog "size"  1.0 x)
	   (set-uniform gpu-prog "color" 1.0 1.0 1.0 1.0)
	   (set-uniform gpu-prog "offset" idx 0.0)
	   (bind-program gpu-prog win)
	   (gl:draw-arrays :quads 0 4)))))

(defun  get-level-at(sea _x _y)
    (with-package enumerate
      (aggregate #'+
	       (select
		(lambda (fcn) (funcall fcn _x _y))
		(walk-list sea)))))


(defun render-new-sea(sea win phase-offset)
  (let ()
	   
    (bind-array vbo win 0)
    (loop for idx from 0 to 400 do
	 (progn
	   (set-uniform gpu-prog "size"  1.0  
					      (get-level-at sea idx phase-offset))
	   (set-uniform gpu-prog "offset" idx 0.0)
	   (bind-program gpu-prog win)
	   (gl:draw-arrays :quads 0 4)))))


       

(defun __mean(&rest args)
  (/ (reduce #'+ args) (length args)))

(let ()
  (defparameter sea2
    (labels ((wavefcn (wave-speed wave-size freq wave-initial-phase)
		      (lambda (x p)
			(progn
			  (setf p (* wave-speed p))
			  (+ 0.3 (* wave-size
			     (- 1
			     (__mean
			      (abs (sin (+ p wave-initial-phase (/ x freq))))
			      (abs (sin (+ p wave-initial-phase (/ (- x 2) freq))))
			      (abs (sin (+ p wave-initial-phase (/ (+ x 2) freq))))
			      ))))))))

    (list 
     (wavefcn 1.0 0.3 15 (utils:random-range 0.0 10.0))
     (wavefcn -0.7 0.3 15 (utils:random-range 0.0 10.0))
     (wavefcn -0.6 0.3 15 (utils:random-range 0.0 10.0))
     (wavefcn -0.2 0.3 15 (utils:random-range 0.0 10.0))
;     (wavefcn 2.9 2.0 2)
     
     (wavefcn 5.0 0.1 3 0)
     ;(wavefcn 0.8 )

     )
;     (lambda (x p) (- 1.0 (expt (- 1 (abs (sin (+ (* 0.04 x) p)))) 0.05) ))
;     (lambda (x p) (- 1.0 (expt (- 1 (abs (sin (+ (* 0.04 x) (* -2.0 p))))) 0.05) ))
;     (lambda (x p) (- 1.0 (expt (- 1 (abs (sin (+ (* 0.06 x) (* -1.0 p))))) 0.05) ))     
;     (lambda (x p) (sin (* 0.1 (+ p x))))
;     (lambda (x p) (cos (* -0.15 (- x p))))
;     (lambda (x p) (min 10.0 
;			(expt 
;			 (cos (* -0.2 (- x (* 10.0 p)))) 
;			 -3.0)))
	  
	  ))
(let ((phase 0.0))
(defun do-render-new-sea (win)
  (let ((n 6.0))
  (loop for x from 0 below n do
       (let ((color (/ x n)))
	 (set-uniform gpu-prog "cameraZoom" 0.005 0.25);(* 2.0 (/1 cam-width)) (/1  cam-height))
	 (set-uniform gpu-prog "cameraPos" 200.0 (+ -1 (* 0.8 x)));center-x center-y)
	 (set-uniform gpu-prog "color" color color color 1.0)
	 (render-new-sea sea2 win (+ phase (* x 0.7)))

    
    (incf phase 0.02)
    ))))
))

(defun get-force-field-vector (force-fields x y)
  (with-package matrix
    (apply #'+ 
	   (mapcar 
	    (lambda (ff) (funcall ff x y)) 
	    force-fields))))

(defun get-liquid-force (liquid-field-function x y r)
  (funcall liquid-field-function x y r))

(defun make-circ-wind-field ()
  (lambda (x y r) 
    (let ((xv (* 2.0 (sin x)))
	  (yv (* 2.0 (cos y))))
      (matrix:make-vector (list xv yv))))

)


(defun make-gravity-field (force)
  (let ((force-vector 
	 (with-package matrix 
	   (make-vector (list 0.0 
			      (coerce force 'single-float))))))
    (lambda (x y)
      force-vector)))

(defun make-circ-field ()
  (lambda (x y)
    (with-package matrix 
      (make-vector (list (sin x)
			 (tan y)
			 )))))

(defstruct particle
  (location (matrix:make-vector '(0.0 0.0)))
  (velocity (matrix:make-vector '(0.0 0.0))))

(defun drag-equation (p v-obj v-fluid C A)
  "p: fluid density, v-obj: object speed, v-fluid: fluid-speed, C coefficient,
A: Cross sectional area"
  (let ((non-matrix-stuff (* 0.5 p C A)))
    (with-package matrix
      (scale (matrix-apply (lambda (mat-cell) 
			     (expt mat-cell 1.0))
			   (-  v-obj v-fluid ))
	     non-matrix-stuff))))

(defun iterate-particles (force-field wind-field particles delta-time)
  (mapcar
   (lambda (particle)
     (let* ((loc (matrix:matrix-data (particle-location particle)))
	    (x (aref loc 0))
	    (y (aref loc 1))
	    (vel (particle-velocity particle))
	    (wind-friction 0.5)
	    (wind-speed (get-liquid-force wind-field x y 0.1))
	    
	    (drag-force (drag-equation 0.1 vel wind-speed 0.5 1.0))
	    )
       ;(print (list wind-speed vel drag-force))
      (make-particle :location 
		      (with-package matrix
			(+ (particle-location particle)
			   (scale (particle-velocity particle) delta-time)))
		      :velocity 
		      (with-package matrix
			(-  vel
			    
			   (scale 
			    (+
			     drag-force
			     (get-force-field-vector force-field x y)) 
			    
			    delta-time))))))
   particles))

;(defparameter force-field (list (make-circ-field) (make-gravity-field -1.2)))
;(defparameter particles 
;  (with-package matrix
;    (list (make-particle :location (make-vector '(0.0 0.0))
;			 :velocity (make-vector '(0.0 0.0))))))

(defun normalized-sin(phase)
  (+ 0.5 (* (sin phase) 0.5)))


(defun make-simulator()
  (let* (
	(time 0.0)
	(force-field4 (list  (make-gravity-field 0.100)))
	(particles3 (with-package matrix
		      (loop for (x y) in 
			   (loop for x from -3.0 to 3.0 append
				(loop for y from -3.0 to 3.0 collect
				     (list x y))) 
			 collect
			   (make-particle :location (make-vector (list x y))
					  :velocity (make-vector '(0.0 0.0))))))
	(wind-field 
	 (lambda (x y r) 
	   (let ((xv (* 2.0 (sin (* 0.4 x))))
		 (yv (* 2.0 (cos (* 0.4  y)))))
	     (matrix:make-vector
	      (if (> y -5)
		  (list 
		   (* 5.0 (- (mod (+ (/ x 3.0) time) 5) 2.5))
		   (* 5.0 (- (mod (+ (/ y 3.0) time) 5) 2.5)))
		  (list 0.0 0.0))
	     ))))
	)
  (lambda (win delta)
    (progn
      (set-uniform gpu-prog "cameraZoom" 0.07 0.07)
      (set-uniform gpu-prog "cameraPos" 0.0 0.0)
      (set-uniform gpu-prog "color" 1.0 1.0 1.0 1.0)
      (bind-array vbo win 0)
      (loop for particle in particles3 
	 for idx from 0 to 1000 do
	   (let ((loc (matrix:matrix-data (particle-location particle)))) 
	     (set-uniform gpu-prog "color" 
			  (normalized-sin idx) 
			  (normalized-sin (* 0.1 idx))
			  (normalized-sin (* 0.5 idx)) 0.5)
	     (set-uniform gpu-prog "size"  0.1 0.1)	  

	     (set-uniform gpu-prog "offset" (aref loc 0) (aref loc 1))
	     (bind-program gpu-prog win)
	     (gl:draw-arrays :quads 0 4)))
      (setf time (+ time delta))
      (setf particles3 (iterate-particles force-field4 wind-field  particles3 delta))))))



(defparameter simulator (make-simulator))
(defparameter first-time t)

(defun key-press (key)
  (let ((edges (remove-if-not
		(lambda (edge) (or (eq (first edge) rnd-idx)
				   (eq (second edge) rnd-idx)))
		(second mesh)))
	(nidx   
     (case key
       (49 0)
       (50 1)
       (51 2)
       (52 3)
       (otherwise 10))))
    (when (> (length edges) nidx)
      (setf rnd-idx
	    (let ((edge (nth nidx edges)))
	      (if (eq (first edge) rnd-idx) 
		  (second edge)
		  (first edge)))))
    (print edges)
    (print key)
    (setf first-time t)
    (setf simulator (make-simulator))
    ))


(defun render-graph(win verts edges idx)
  (progn
    (set-uniform gpu-prog "cameraZoom" 0.2 0.2)
    (set-uniform gpu-prog "cameraPos" 0.0 0.0)
    (bind-array line-vbo win 0)
    (loop for (a b) in edges do
	 (let* ((a-vert (nth a verts))
		(b-vert (nth b verts))
		(diff-vert (mapcar #'- b-vert a-vert)))
	   (set-uniform gpu-prog "offset"  (first a-vert) 
			(second a-vert))
	   (set-uniform gpu-prog "size" (first diff-vert)
			(second diff-vert))
	   (set-uniform gpu-prog "color" 1.0 1.0 1.0 1.0)


	   (bind-program gpu-prog win)
	   (gl:draw-arrays :lines 0 2)))
    (bind-array vbo win 0)
    (set-uniform gpu-prog "size"  0.2 0.2)
    (let ((target-vert (nth idx verts)))
      (set-uniform gpu-prog "offset" (first target-vert)
		   (second target-vert))
      (bind-program gpu-prog win)
      (gl:draw-arrays :quads 0 4))
    ))

(defun gen-test-graph ()
  (let ((vertexes '((0.0 0.0) 
		    (-1.0 3.0) 
		    (-2.0 1.0)
		    (1.0 1.0)
		    (3.0 3.0)
		    (3.0 4.0)
		    (2.0 5.0)))
	(indices '((0 1) (1 2) (2 0) (2 3) (1 3) (0 3) (3 4) (4 5) (5 6) (6 1))))
    (list vertexes indices)))


(defparameter mesh (gen-test-graph))
(defparameter rnd-idx 0)
(defun render (win)
  (sleep 0.002)
  (glfwshowwindow (slot-value win 'glfw-window-ref))
  (in-window win
	     (progn
	       
	       (gl:clear-color 0.0 0.0 0.0 1.0)
	       (when (utils:post-set first-time nil)
		 (gl:clear :color-buffer-bit)
		 (swap-buffers win)
		 (gl:clear :color-buffer-bit)
		 (swap-buffers win)
		 
		 )
		 (gl:clear :color-buffer-bit)
		 
	       (gl:enable :blend)
	       (setf itasd(+ itasd dit))
	       (unless (> 1 (+ dit itasd) 0.3)
		 (setf dit (- dit)))
		 
	       (gl:blend-func :src-alpha :one-minus-src-alpha)
	       
					;(funcall simulator win 0.05)
	       ;(do-render-new-sea win)
	       ;(render-sea sea win)
	       
	       (render-graph win (first mesh) (second mesh) rnd-idx)
	       (swap-buffers win)
	       (glfwpollevents))))

(defun gen-test-verts ()
  (let ((vertexes '((0.0 0.0 0.0) 
		    (1.0 1.0 1.0) 
		    (0.0 1.0 0.0)
		    (0.0 1.0 1.0)))
	(indices '((0 1 2) (2 3 4))))
    (list vertexes indices)))



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

(defun render (win)
  (sleep 0.01)
  (glfwshowwindow (slot-value win 'glfw-window-ref))
  (in-window win
	     (progn
	       (gl:clear-color 0.0 0.0 0.0 1.0)
	       (gl:clear :color-buffer-bit)
	       (gl:disable :blend)
	       (setf itasd(+ itasd dit))
	       (unless (> 1 (+ dit itasd) 0.3)
		 (setf dit (- dit)))
		 
	       (gl:blend-func :one :one)
	       (do-render-new-sea win)
	       ;(render-sea sea win)
	       (swap-buffers win)
	       (glfwpollevents))))



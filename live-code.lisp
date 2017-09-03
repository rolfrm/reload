

(defvar win nil)
(defvar inited nil)

(defun set-blend-mode()
  (in-window win
	     (progn
	       (gl:clear-color 0.0 0.0 0.0 1.0)
	       (gl:enable :blend)
	       (gl:blend-func :src-alpha :dst-alpha) ;;Additive alpha blending
	       (gl:blend-func :one :one) ;;Additive blending
	       ;(gl:blend-func :zero :src-alpha) ;;Additive (1 - alpha) blending
	       ;(gl:blend-func :src-alpha :one-minus-src-alpha) ;transparancy
	       ;(gl:blend-func :one-minus-src-alpha :src-alpha)
	       ;(gl:clear :color-buffer-bit)
	       )))

(defun tryinit()
  (progn
    (unless inited
      (progn 
	(setf inited t)
	(glfwInit)
	(setf win (make-instance 'window :width 250 :height 250 :title "asd"))
	(add-listener (slot-value win 'mouse-move-event) (lambda (x y) (print (list x y))))))))
	

    

(defvar frag-obj (make-instance 'gpu-program-object :kind :fragment))
(defvar vert-obj (make-instance 'gpu-program-object :kind :vertex))


(set-shader-code frag-obj "
uniform vec4 color;
uniform sampler2D sampler;
uniform sampler2D sampler2;
varying vec2 uv;
void main(){
  
  gl_FragColor = color*vec4(1 - uv*1.0,0.3,1.0);// + texture2D(sampler,uv);// + texture2D(sampler2,uv);
}")
(set-shader-code vert-obj "
attribute vec2 pos;
uniform vec2 offset;
varying vec2 uv;
uniform vec2 size;
void main(){
  uv = pos / 1.0;
  gl_Position = vec4(pos*size + offset ,0.0,1.0);

}
")


(defvar gpu-prog (make-instance 'gpu-program :vertex vert-obj :fragment frag-obj))
(defvar vbo (make-instance 'gpu-array))
(set-vertexes vbo (list -1.0 -1.0 1.0 -1.0 1.0 1.0 -1.0 1.0))
(set-dimension vbo 2)

(defun correct-sin (x)
  (* 0.5 (+ x 1.0)))
(defun distance (x1 x2)
  (let ((x (- x1 x2)))
    (abs x)))
;(defparameter scale 100.0)
(defvar spring 1.0)
;(setf spring 1000)
(setf spring-target 100)
(setf spring-speed 0.0)

(defun draw (n2)
  ;(setf scale (+ 0.1 (* n2 1.001)))
  (setf spring (+ spring  spring-speed))
  (setf spring-speed (+ spring-speed (* 0.1 (if (< 0 (- spring-target spring)) 1.0 -1.0))))
  (setf spring-speed (* 0.99 spring-speed))
  ;(print (format nil "~a ~a ~a" spring spring-target spring-speed))
					;(setf spring (+ spring (/ (- spring-target spring) 32.0)))  
  ;(if (< (distance spring spring-target) 0.01)
   ;   (setf spring-target 100.0))

  ;(print spring)
  (dotimes (n (round 25))
    (let* (
	   (y (mod n 5)) 
	   (x (floor (/ n 5)))
	   (r (+ 2.0 (sin (/ lasty 20)) (/ (sin it) 10.0)  (/ spring 100)))
	   (g (/ (sin (+ n2 x (/ lasty 100))) 1.0))
	   (d (cos (+ n2 (+ it))))
	   (b (abs d))
	   )
      (set-uniform gpu-prog "size" 
		   (- 0.2 (/ (correct-sin r) 5.0)) 
		   (- 0.2 (/ (correct-sin r) 5.0)))
      (set-uniform gpu-prog "color" (abs r) (abs g) (abs b) 0.5)
      (set-uniform gpu-prog "offset" 
		   (- (- (/ x 2.5) 0.8) 0.0) 
		   (- (- (/ y 2.5) 0.8) 0.0))

      
      (bind-program gpu-prog win)
      (bind-array vbo win 0)
      
      (in-window win
		 (gl:draw-arrays :triangle-fan 0 4))
      
      (set-uniform gpu-prog "color" 1.0 1.0 1.0 1.0)
      (bind-program gpu-prog win)
      
      (bind-array vbo win 0)
      
      (in-window win
      (gl:draw-arrays :line-loop 0 4))))
;      ))
)


(defvar running nil)
(setf running t)
(defvar it 1)
(defvar lastx 0)
(defvar lasty 0)
(defvar last-error ())
(when win
  (clear-listeners (slot-value win 'mouse-move-event))
  (add-listener (slot-value win 'mouse-move-event) 
		(lambda (x y) 
		  (progn
		    (setf it (+ it (/ (- x lastx) 50)))
		    (setf lastx x)
		    (setf lasty y))))
  (clear-listeners (slot-value win 'mouse-click-event))
  (add-listener (slot-value win 'mouse-click-event) 
		(lambda (x y) 
		  (progn
		    (print (format nil "~a ~a" x y))
		    (setf spring-target (* 300 (- (- 1 y) 0.9)))
		    (setf spring (+ spring (/ (- spring-target spring) 2.0)))  ))))



(defun continous-update ()
  (tryinit)
  (handler-case
      (progn
	(glfwshowwindow (slot-value win 'glfw-window-ref))
	(in-window win (gl:clear :color-buffer-bit))  
	(set-blend-mode)
	(draw it)
	;(draw (+ 1 it))
	;(draw (+ 3 it))
	;(draw (+ 5 it))
	;(draw (+ 7 it))
	;(draw (+ 20 it))
	(swap-buffers win)
	(glfwPollEvents)
	)
    (error (e)
      (unless (equal last-error e)
	(setf last-error e)
	(print e))))
  (setf it (+ it 0.1))
  (sleep 0.01)
  (when running
    ;(continous-update)))
    (begin-invoke dispatch #'continous-update)))
;(begin-invoke dispatch #'continous-update)
(print (slot-value dispatch 'event-queue))
(defvar a 0)

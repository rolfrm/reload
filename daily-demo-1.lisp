
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
uniform float cameraZoom;
varying vec2 uv;
uniform vec2 size;
void main(){
  uv = pos / 1.0;
  gl_Position = vec4((pos*size + offset - cameraPos)*cameraZoom ,0.0,1.0);

}
")

(defvar gpu-prog (make-instance 'gpu-program :vertex vert-obj :fragment frag-obj))
(defvar vbo (make-instance 'gpu-array))
(set-vertexes vbo (with-package enumerate
		    (to-list (select-many-list (lambda (x) (list (sin x) (cos x))) 
					   (select (lambda (x) (* x (/ (coerce pi 'single-float) 10.0))) 
						   (range 20))))))


(defvar itasd 0.5)
(defparameter dit 0.01)
(defun render(win)
  (sleep 0.01)
  (glfwshowwindow (slot-value win 'glfw-window-ref))
  (in-window win
	     (progn
	       (gl:clear-color 0.0 0.0 0.0 1.0)
	       (gl:clear :color-buffer-bit)
	       (gl:enable :blend)
	       (setf itasd(+ itasd dit))
	       (unless (> 1 (+ dit itasd) 0.3)
		 (setf dit (- dit)))
		 
	       (gl:blend-func :src-alpha :one)
	       
	       (set-uniform gpu-prog "cameraZoom" 1.0)
	       (bind-array vbo win 0)
	       (with-package enumerate
		 (for-each (lambda ( x2)
			     (progn
			       (set-uniform gpu-prog "size" 0.3 0.3)
			       (set-uniform gpu-prog "color" (- 1.0 x2) (cos (+ x2 itasd)) (sin itasd) 0.3)
			       (set-uniform gpu-prog "offset"  x2 x2)
			       (bind-program gpu-prog win)
			       (gl:draw-arrays :triangle-fan 0 20)
			       
			       (set-uniform gpu-prog "size" 0.08 0.1)
			       (set-uniform gpu-prog "color" 0.2 0.2 1.0 0.3)
			       (set-uniform gpu-prog "offset"  (- x2 0.05) x2)
			       (bind-program gpu-prog win)
			       (gl:draw-arrays :triangle-fan 0 20)

			       (set-uniform gpu-prog "size" 0.05 0.05)
			       (set-uniform gpu-prog "color" 0.2 0.2 0.0 1)
			       (set-uniform gpu-prog "offset"  (+ 0.1 x2) (- x2 0.05))
			       (bind-program gpu-prog win)
			       (gl:draw-arrays :triangle-fan 0 20)

			       (set-uniform gpu-prog "size" 0.2 0.05)
			       (set-uniform gpu-prog "color"  0.2 0.2 1.0 0.3)
			       (set-uniform gpu-prog "offset"  x2 (- x2 0.2))
			       (bind-program gpu-prog win)
			       (gl:draw-arrays :triangle-fan 0 20)
			     ))
			   
			   
			   (select (lambda (x) (- (/ x 2.0) 0.5))
				   (range 3))))
	       (swap-buffers win)
	       (glfwpollevents)))
)


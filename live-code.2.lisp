
(print "running live-code.lisp")

(print "ok this seems to work")
(defvar win)
(defvar inited nil)
(unless inited
  (progn 
    (setf inited t)
    (glfwInit)
    (setf win (make-instance 'window :width 250 :height 250 :title "asd"))
    (add-listener (slot-value win 'mouse-move-event) (lambda (x y) (print (list x y))))
    ))
    
(in-window win
	   (progn
	     (gl:clear-color 0.0 0.0 0.0 1.0)
	     (gl:enable :blend)
	     (gl:blend-func :src-alpha :dst-alpha) ;;Additive alpha blending
	     ;(gl:blend-func :one :one) ;;Additive blending
	     ;(gl:blend-func :zero :src-alpha) ;;Additive (1 - alpha) blending
	     ;(gl:blend-func :src-alpha :one-minus-src-alpha) ;transparancy
	     ;(gl:blend-func :one-minus-src-alpha :src-alpha)
	     (gl:clear :color-buffer-bit)
	     ))
(defvar frag-obj (make-instance 'gpu-program-object :kind :fragment))
(defvar vert-obj (make-instance 'gpu-program-object :kind :vertex))


(set-shader-code frag-obj "
uniform vec4 color;
uniform sampler2D sampler;
uniform sampler2D sampler2;
varying vec2 uv;
void main(){
  
  gl_FragColor = color;// + texture2D(sampler,uv);// + texture2D(sampler2,uv);
}")
(set-shader-code vert-obj "
attribute vec2 pos;
uniform vec2 offset;
uniform float z;
varying vec2 uv;

void main(){
  uv = pos / 8.0;
  gl_Position = vec4(pos*0.1 - offset*1.1 + vec2(0.4,0.4),0.0,z);

}
")


(defvar gpu-prog (make-instance 'gpu-program :vertex vert-obj :fragment frag-obj))
(defvar vbo (make-instance 'gpu-array))
(set-vertexes vbo (list -1.0 -2.0 2.0 -2.0 2.0 2.0 -2.0 2.0))
(set-dimension vbo 2)

(defparameter scale 50.0)
(dotimes (n (round scale))
  (set-uniform gpu-prog "z" (* (- 1.6 (/ n scale)) 1.6)) 
  (set-uniform gpu-prog "color" (- 1.2 (/ n scale)) (/ n scale)  (- 1.2 (/ n scale)) 0.4)
  (set-uniform gpu-prog "offset" (- 1 (/ n scale)) (/ n scale))
  (bind-program gpu-prog win)
  (bind-array vbo win 0)
  (in-window win
	     (gl:draw-arrays :triangle-fan 0 4))
  (set-uniform gpu-prog "color" 0.3 0.3 0.3 1)
 ;(set-uniform gpu-prog "z" (- 1 (/ n scale))) 

 ;(set-uniform gpu-prog "offset" (- 1 (/ n scale)) (/ n scale))
  (bind-program gpu-prog win)
  (bind-array vbo win 0)
  (in-window win
	     (gl:draw-arrays :line-loop 0 4)))

(swap-buffers win)

(defvar a 0)
(defun update (t2)
  (when (> (incf a) 10)
    (progn (print t2)
	   (setf a 0))))



(glfwInit)
(defvar frag)
(defvar vert)
(defvar _prog)
(defvar _vbo)
(setf _vbo (make-instance 'vbo :vertexes (list 0.0 0.0 0.0 1.0 1.0 1.0 1.0 0.0) :dim 2))
(setf frag (make-instance 'shader-string :shader-kind :fragment))
(setf vert (make-instance 'shader-string :shader-kind :vertex))
(setf _prog (make-instance 'shader-program :fragment frag :vertex vert))

(defparameter fragment-code "

uniform vec4 color;

void main(){

  gl_FragColor = vec4(1.0,1.0,1.0,1.0) - color;
}
")

(defparameter vertex-code "
attribute vec2 pos;
void main(){
  gl_Position = vec4(pos*0.5,0.0,1.0);
}

")

(setf (slot-value frag 'shader-code) fragment-code)
(setf (slot-value vert 'shader-code) vertex-code)

(defvar win3)

(defvar win)
(defvar win2)

;(setf win (glfwCreateWindow 20 200 "asd2" (cffi:null-pointer) (cffi:null-pointer)))
;(setf win2 (glfwCreateWindow 100 100 "win2" (cffi:null-pointer) win))
(setf win (make-instance 'window :title "asd2" :width 250 :height 250 :x 0 :y 0))
(setf win2 (make-instance 'window :title "win2" :width 250 :height 250 :other-window win :x 250 :y 0))

(make-context-current win2)
(gl:clear-color 0.0 0.0 0.0 1.0)
(make-context-current win)
(gl:clear-color 1.0 1.00 1.0 1.0)

(defparameter vertexes (make-instance 'gpu-array))
(set-vertexes vertexes (list 0.0 0.0 0.0 1.0 1.0 1.0 0.0))
(set-dimension vertexes 2)

(defparameter frag-obj (make-instance 'gpu-program-object :kind :fragment))
(set-shader-code frag-obj fragment-code)

(defparameter vert-obj (make-instance 'gpu-program-object :kind :vertex))
(set-shader-code vert-obj vertex-code)

(defparameter gpu-prog1 (make-instance 'gpu-program :vertex vert-obj :fragment frag-obj))

(defparameter m 10.0)
(dotimes (n (round m))
  (setf (slot-value _vbo 'vertexes) (list 0.0 0.0 0.0 1.0 1.0 1.0 (- (/ n m) 1) 0.0))
  (make-context-current win)
  (gl:clear :color-buffer-bit)
  
  (draw win vertexes gpu-prog1)
  ;(set-uniform _prog "color" (/ n m) (/ n m) (/ n m) 0.0)
  ;(load-shader _prog)
  ;(load-data _vbo)
  ;(load-vbo _vbo 0)
  ;(gl:draw-arrays :quads 0 4) 
  
  (make-context-current win2)
  (gl:clear :color-buffer-bit)
  (load-shader _prog)
  (load-data _vbo)
  (load-vbo _vbo 0)
  (gl:draw-arrays :quads 0 4) 
  
  (swap-buffers win)
  (swap-buffers win2)
  (sleep 0.001))
(destroy-window win)
(destroy-window win2)
(glfwTerminate)


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
(set-vertexes vbo '(0.0 0.0 1.0 1.0))

(defstruct l-tree
  (size (utils:random-range 0.5 1.9) :type single-float)
  (angle (utils:random-range -0.7 0.7) :type single-float)
  (sub-trees '()))



(defun make-test-l-tree(depth &optional (size-scale 1.0))
  (make-l-tree :size (* size-scale (utils:random-range 0.2 0.8)) 
	       :sub-trees 
               (if (eq depth 0)
                   nil
                   (with-package enumerate (to-list (select (lambda (x) (make-test-l-tree (- depth 1)
 (expt size-scale 2.0)))
                                                    (range 2)))))))


(defun render-l-tree (l-tree loc angle lmb)
  (let* ((ang (+ angle (l-tree-angle l-tree)))
         (size (l-tree-size l-tree))
         (vec (with-package matrix (+ loc (* (make-vector (list size size)) (make-vector  (list (sin ang)
                                                              (cos ang))))))))
    (funcall lmb loc vec)
    (loop for child in (l-tree-sub-trees l-tree) do
         (render-l-tree child vec angle lmb))))
    

(defvar itasd 0.5)
(defparameter dit 0.01)
(defparameter ltree (make-test-l-tree 1 0.9))
(defparameter ltree2 (make-test-l-tree 2 0.9))
(defparameter ltree3 (make-test-l-tree 3 0.9))
(defparameter ltree4 (make-test-l-tree 3 0.9))
(defparameter ltree5 (make-test-l-tree 4 0.9))

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
	       
	       (set-uniform gpu-prog "cameraZoom" 0.4)
	       (set-uniform gpu-prog "cameraPos" 0.0 2.0)
	       (bind-array vbo win 0)
	       (mapcar (lambda (ltree x y) (render-l-tree  ltree (matrix:make-vector (list x y)) 0.0 
			       (lambda (x2 y2)
				 (let ((x (matrix:matrix-data x2))
				       (y (matrix:matrix-data y2)))
				   (set-uniform gpu-prog "size"  (- (aref y 0) (aref x 0)) (- (aref y 1) (aref x 1)))
				   (set-uniform gpu-prog "color" (- 1.0 0) (cos (+ 0 itasd)) (sin itasd) 0.3)
				   (set-uniform gpu-prog "offset" (aref x 0) (aref x 1))

				   (bind-program gpu-prog win)
				   (gl:draw-arrays :line-loop 0 2)
				   
				   )))) (list ltree ltree2 ltree3 ltree4 ltree5) 
					(list -2.0 -1.0 0.0 1.0 2.0) 
					(list 0.0 0.0 0.0 0.0 0.0) )		   
	       (swap-buffers win)
	       (glfwpollevents))))



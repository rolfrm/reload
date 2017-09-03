(defvar frag-obj (make-instance 'gpu-program-object :kind :fragment))
(defvar vert-obj (make-instance 'gpu-program-object :kind :vertex))
(set-shader-code frag-obj "
uniform vec4 color;
uniform sampler2D sampler;
varying vec2 uv;
void main(){
  gl_FragColor = texture2D(sampler,uv); //color;// - texture2D(sampler,uv);
}")
(set-shader-code vert-obj "
attribute vec2 pos;
uniform vec2 offset;
uniform vec2 cameraPos;
uniform float cameraZoom;
varying vec2 uv;
uniform vec2 size;
uniform vec2 uv_start;
uniform vec2 uv_stop;
void main(){
  vec2 nuv =  vec2(pos.x , 1 - pos.y);
  uv = nuv * (uv_stop - uv_start) + uv_start;
  gl_Position = vec4((pos*size + offset - cameraPos)*cameraZoom ,0.0,1.0);
}
")

(defvar shader2-vert 
"
attribute vec3 vertex_position;
attribute vec2 uv_cooedinate;
uniform mat4 camera_transform;
uniform mat4 model_transform;
void main(){
}
")

(defvar gpu-prog (make-instance 'gpu-program :vertex vert-obj :fragment frag-obj))

(defvar empty-frag-obj (make-instance 'gpu-program-object :kind :fragment))
(defvar empty-vert-obj (make-instance 'gpu-program-object :kind :vertex))

(defparameter empty-prog (make-instance 'gpu-program :vertex empty-vert-obj :fragment empty-frag-obj))

(defvar vbo (make-instance 'gpu-array))
(vbo:set-vertexes vbo '(0.0 0.0 1.0 1.0))

(defvar square-vbo (make-instance 'gpu-array))
(set-vertexes square-vbo '(0.0 0.0
			   0.0 1.0
			   1.0 1.0
			   1.0 0.0))

;; Camera state / graphics
(defstruct render-view
  (camera-zoom 1.0)
  (camera-position (matrix:make-vector '(0.0 0.0 0.0)))
  (window-size (matrix:make-vector '(1.0 1.0))))

(defun window-pos-to-screxen-pos (mouse-pos render-view)
  (let* ((window-size (render-view-window-size render-view)))
    (with-package matrix
      (* 
       (make-vector'(1.0 -1.0))
       (- 
	(scale (/ mouse-pos window-size) 2.0)
	(make-vector '(1.0 1.0))))
      )))

(defun world-to-screen-project(vec render-view)
  (with-slots(camera-zoom camera-position) render-view
    (matrix:make-vector 
     (with-package enumerate
       (to-list (take 2 (walk-array (matrix:matrix-data
			 (matrix:scale (matrix:-  
				  vec
				 camera-position)
				camera-zoom)
			 )))))
     )))

(defun screen-to-world-project(screen-vector projection-point render-view)
  (with-slots (camera-zoom camera-position) render-view
    (let ((inverse-zoom (/ 1.0 camera-zoom)))
      (matrix:+
       (matrix:scale
	(matrix:make-vector 
	 (list (matref screen-vector 0) 
	       (matref screen-vector 1)
	       (matref projection-point 2)))
	inverse-zoom)
       camera-position))))


;;functions for finding vertexes on click UI -> game
(defun get-vertex-screen-iterator (grph point render-view)
  (with-package enumerate 
    (order-by 
     (walk (graph-vertexes grph)) 
     :key (lambda (vertex)
	    (let ((pt (matrix:- 
		       
		       (world-to-screen-project (vertex-location vertex) 
						 render-view)
		       point)))
	      (matref (matrix:dot (matrix:transpose pt) pt) 0)))
     )
    ))

(defun get-vertex-edge-screen-iterator (grph point render-view)
  (with-package enumerate 
    (order-by 
     (walk (graph-vertexes grph)) 
     :key (lambda (vertex)
	    (let ((pt (matrix:- (world-to-screen-project (vertex-location vertex) render-view)
				point)))
	      (matref (matrix:dot (matrix:transpose pt) pt) 0)))
     )
    ))


(defun get-nearest-screen-vertexes (grph point max-distance n-vertexes render-view)
  (with-package enumerate
    (to-list
     (take n-vertexes
	   (where
	    (lambda (distance) t)
	    (get-vertex-screen-iterator grph point render-view))))))

(defun get-nearest-screen-vertex (grph point max-distance render-view)
  (first (get-nearest-screen-vertexes grph point max-distance 1 render-view)))

(defparameter test-fragment-code "

uniform vec4 color;
uniform sampler2D sampler;
uniform sampler2D sampler2;
varying vec2 uv;
void main(){

  gl_FragColor = vec4(1.0,1.0,1.0,1.0) - texture2D(sampler,uv);// + texture2D(sampler2,uv);
}
")

(defparameter test-vertex-code "
attribute vec2 pos;
varying vec2 uv;
void main(){
  uv = pos / 2.0;
  gl_Position = vec4(pos*0.08,0.0,1.0);
}

")

(defun test-context-manager ()
  (progn
    (glfwinit)
    (unwind-protect
	 (let ((win (make-instance 'window :title "asd" :width 250 :height 250 :x 0 :y 0)))
	   (make-context-current win)
	   (gl:clear-color 0.2 0.3 0.4 1.0)
	   (gl:clear :color-buffer-bit)
	   (let* ((vertexes (make-instance 'gpu-array)) 
		  (frag-obj (make-instance 'gpu-program-object :kind :fragment))
		  (vert-obj (make-instance 'gpu-program-object :kind :vertex))
		  (gpu-prog1 (make-instance 'gpu-program :vertex vert-obj :fragment :frag-obj)))
	     (dotimes (n 5)
	       (gl:clear :color-buffer-bit)
	       (bind-array vertexes win 0)
	       (use-program gpu-prog1 win)
	       (draw-arrays 0 4 :lines win)
	       (swap-buffers win)
	       (sleep 0.2))
	     (describe (slot-value win 'context-manager))))
      
    (glfwTerminate))))

(defun cycle (p)
  (+ (/ (sin p) 2) 0.5))

(defun test-shader-and-window ()
  (progn
    (print "glfwInit")
    (glfwInit)
    (print "done")
    (unwind-protect
	 (let* ((win (make-instance 'window :title "asd" :width 250 :height 250 :x 500 :y 0))
	       (win3 (make-instance 'window :other-window win :title "win3" :width 50 :height 50 :x 850 :y 0))
	       (win4 (make-instance 'window :other-window win :title "win4" :width 500 :height 500 :x 0 :y 0))
		(win2 (make-instance 'window :other-window win :title "win2" :width 100 :height 100 :x 750 :y 0)))
	   
	   (in-window win
	   	      (gl:clear-color 0.2 0.1 0.0 1.0))
	   (make-context-current win2)
	   (gl:clear-color 0.1 0.1 0.4 1.0)
	   (make-context-current win3)
	   (gl:clear-color 0.2 0.3 0.1 1.0)
	   (make-context-current win4)
	   (gl:clear-color 0.2 0.1 0.1 1.0)
	   
	   (let* ((frag-obj (make-instance 'gpu-program-object :kind :fragment))
		  (vert-obj (make-instance 'gpu-program-object :kind :vertex))
		  (gpu-prog1 (make-instance 'gpu-program :vertex vert-obj :fragment frag-obj))
		  (vbo (make-instance 'gpu-array)))
	     (set-shader-code frag-obj test-fragment-code)
	     (set-shader-code vert-obj test-vertex-code)
	     (set-vertexes vbo (list 0.0 0.0 0.0 1.0 1.0 1.0 1.0 0.0))
	     (set-dimension vbo 2)
	     (dotimes (n 600)
	       (let ((x (/ (- n 200) 4)))
		 (set-vertexes vbo (list (/ x 5.0) (/ x 5.0) 0.0 10.0 10.0 10.0 10.0 0.0))
		 ;(set-uniform gpu-prog1 "color" (cycle (+ 1.3 (/ n 10))) (cycle (/ n 10)) (cycle (+ 2.6 (/ n 10))) 0.0)
		 (bind-array vbo win2 0)
		 ;(gl:clear :color-buffer-bit)
		 (bind-program gpu-prog1 win2)
		 (gl:draw-arrays :quads 0 4)
		 (swap-buffers win2)
		 
		 (bind-array vbo win3 0)
		 ;(gl:clear :color-buffer-bit)
		 (bind-program gpu-prog1 win3)
		 (gl:draw-arrays :points 0 4)
		 (swap-buffers win3)

		 (bind-array vbo win4 0)
		 ;(gl:clear :color-buffer-bit)
		 (bind-program gpu-prog1 win4)
		 (gl:draw-arrays :line-loop 0 4)
		 (swap-buffers win4)
		 
		 (bind-array vbo win 0)
		 ;(gl:clear :color-buffer-bit)
		 (bind-program gpu-prog1 win)
		 (gl:draw-arrays :lines 0 4)
		 (swap-buffers win)
		 (sleep 0.05)))))
      (glfwTerminate))))
(defun gl-enum-value (enum)
  (cffi:foreign-enum-value '%gl:enum enum))

(defun flatten-png-image (im)
  (let ((width (png:image-width im))
	(height (png:image-height im))
	(channels (png:image-channels im)))
    (let* ((len (* width height channels))
	   (arr (make-array len :element-type '(unsigned-byte 8))))
      (dotimes (idx len)
	(setf (aref arr idx) (row-major-aref im idx)))
	   arr)))
      
      
	     
(defun load-test-texture ()
  (let ((test-im 
	(with-open-file (input "test.png" :element-type '(unsigned-byte 8))
	  (png:decode input))))
    (let ((reference (first (gl:gen-textures 1)))
	  (flattened (flatten-png-image test-im)))
      (print "loading texture..")
      (gl:bind-texture :texture-2d reference)
      (gl:tex-parameter :texture-2d :texture-min-filter :linear)
      (gl:tex-parameter :texture-2d :texture-mag-filter :linear)
      (gl:tex-image-2d :texture-2d 0 :rgb 16 16 0 :rgb :unsigned-byte flattened)
      (print "end..")
      reference)))
(defun load-test-texture-data ()
  (with-open-file (input "test.png" :element-type '(unsigned-byte 8))
    (flatten-png-image (png:decode input))))

(defun test-texturing()
  (progn
    (glfwInit)
    (unwind-protect
	 (let* ((win (make-instance 'window :title "asd" :width 500 :height 500 :x 0 :y 0)))
	   
	   
	   (in-window win
	   	      (progn (gl:clear-color 0.2 0.1 0.0 1.0)
			     (gl:enable :texture-2d)))
	   
	   (let* ((frag-obj (make-instance 'gpu-program-object :kind :fragment))
		  (vert-obj (make-instance 'gpu-program-object :kind :vertex))
		  (gpu-prog1 (make-instance 'gpu-program :vertex vert-obj :fragment frag-obj))
		  (vbo (make-instance 'gpu-array))
		  (texobj (make-instance 'raw-texture :width 16 :height 16
					 :channels 3 :data (load-test-texture-data))))
	     (set-shader-code frag-obj test-fragment-code)
	     (set-shader-code vert-obj test-vertex-code)
	     (set-vertexes vbo (list 0.0 0.0 0.0 1.0 1.0 1.0 1.0 0.0))
	     (set-dimension vbo 2)
	     (dotimes (n 600)
	       (let ((x (/ (- n 200) 4)))
		 (set-vertexes vbo (list (/ x 5.0) (/ x 5.0) 0.0 10.0 10.0 10.0 10.0 0.0))
		 (set-uniform gpu-prog1 "color" (cycle (+ 1.3 (/ n 10))) (cycle (/ n 10)) (cycle (+ 2.6 (/ n 10))) 0.0)
		 
		 (bind-texture texobj win 2)
		 
		 (bind-array vbo win 0)
		 
		 ;(gl:clear :color-buffer-bit)
		 
		 (bind-program gpu-prog1 win)
		 (gl:draw-arrays :quads 0 4)
		 (swap-buffers win)
		 (let ((tdata (get-data texobj))
		       (idx (mod (* n 2) (* 16 16 3))))
		   (setf (aref tdata idx) 0)
		   (setf (aref tdata (+ idx 1)) 0)
		   (setf (aref tdata (+ idx 2)) 0) 
		   (set-data texobj tdata))
		 (sleep 0.05)))))
      (glfwTerminate))))

(defun load-and-go()
  (progn
    ;(load "glres.lisp")
    (load "test-context-manager.lisp")
    (test-context-manager)))
  
(defun test-code-reloader()
  (let ((glInited nil)(reloaderInited nil))
    (unwind-protect
	 (progn
	   (glfwInit)
	   (setf glInited t)
	   (let ((reloader (make-instance 'file-watcher :path "live-code.lisp" (make-evt-handler))))
	     (setf reloaderInited reloader)))
      (progn
	(when glInited
	  (glfwTerminate))
	(when reloaderInited
	  (dispose-file-watcher reloaderInited)))))) 
   

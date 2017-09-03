(re.load "item-manager.lisp")

(defpackage :reactive-ui (:use :cl :reload-engine)
	    (:nicknames :rui)
	    (:export :render-ui :material :scene-node :camera :uniform :transform :create-window :shader :buffer )
	    )
(in-package :reactive-ui)

(defstruct scene-graph 
  (parent) ;allow traveling up.
  (element)
  (child-elements))

;;creates a node in a scene-graph. Can have node children
(defun scene-node (node-object &rest sub-nodes)
  (make-scene-graph :element node-object
		    :child-elements sub-nodes))


(defclass material ()
  ((texture :initform nil :initarg :texture)
   (color :initform (m:vec 1.0 1.0 1.0 1.0) :initarg :color)))
(defclass camera ()
  ((size :initform (m:vec 1 1) :initarg :size)))
(defstruct transform
  (rotation 0.0)
  (translation (m:vec 0.0 0.0))
  (scale (m:vec 1.0 1.0))
  )

(defstruct polygon
  (vert (m:matrix))
  (uv (m:matrix))
  (draw-kind :quads))
(defstruct buffer
  (values (m:matrix))
  (program-indentifier 0))
(defstruct uniform
  (name "")
  (values nil))
(defstruct shader
  (vertex-code "")
  (fragment-code ""))
(defstruct draw
  (draw-kind :quads))


(defun material (&key (texture nil) (color (m:vec 1.0 1.0 1.0 1.0)))
  (make-instance 'material :texture texture :color color))
(defun camera (&key (size (m:vec  1 1)))
  "Width and height of nil means same as window"
  (make-instance 'camera :size size))
(defun transform (&key 
		    (rotation (m:vec 0.0 0.0))
		    (scale (m:vec 1.0 1.0))
		    (translation (m:vec 0.0 0.0)))
  (make-instance 'transform 
		 :rotation rotation
		 :scale scale
		 :translation translation))
(defun buffer (matrix &key (shader-identifier 0))
  (make-buffer :values matrix
	       :program-identifier shader-identifier))
   
(defun make-draw (&key (kind :quads))
  (make-draw :draw-kind kind))

(defun uniform (name &rest values)
  (make-uniform :name name :values values))

(defun set-context-uniform (uniform context))

(defmethod load-graph-elem ((uni uniform) context)
  (set-context-uniform uni context))

(defclass shader-prog ()
  ((shader-ref :initform 0)
   (vertex-ref :initform 0)
   (fragment-ref :initform 0)))

(defun load-shader(shader-proto))
(defun delete-shader (shader))
(defun hash-shader (shader-proto))

(defvar *shader-manager* (item-manager :max-items 5 :load-fcn #'load-shader :delete #'delete-shader 
				       :hash #'hash-shader))
(defun load-texture(path))
(defun delete-texture(path tex))
(defvar *texture-manager* (item-manager :max-items 10 :load-fcn #'load-texture :delete #'delete-texture))


(defmethod load-graph-elem ((shader shader) context)
  (add-assoc 'shader shader context))

(defmethod load-graph-elem((mat material) context)
  (add-assoc 'render-target mat (add-assoc 'material mat context)))

(defmethod load-graph-elem((cam camera) context)
  (let ((render-target (get-assoc 'render-target context)))
    ()))

(defun rotate-vector (vec r)
  (let ((cosr (cos r))
	(sinr (sin r))
	(x (m:ref vec 0))
	(y (m:ref vec 1)))
    (m:vec (- (* x cosr) (* y sinr))
	   (+ (* x sinr) (* y cosr)))))

(defmethod load-graph-elem ((tform transform) context)
  (let ((t (get-assoc 'transform context))
	(r (get-assoc 'rotation context))
	(s (get-assoc 'scale context)))
    (let ((tt (transform-translation tform))
	  (tr (transform-rotation tform))
	  (ts (transform-scale tform)))
      (add-assoc 'transform
		 (m:+ t (m:* s (rotate-vec tt r)))
		 (add-assoc 'rotation (+ r tr)
			    (add-assoc 'scale (+ s ts) context))))))

(defstruct vbo
  (dim 0)
  (length 0)
  (ref 0))

(defun load-vbo (matrix)
  (let ((shape (m:get-2d-shape matrix))
	(ref (first (gl:gen-buffers 1)))
	(data (m:matrix-data matrix)))
    (gl:bind-buffer :array-buffer ref)
    (gl:with-gl-array (arr :float :count (first shape))
      (dotimes (i (* (second shape) (first shape)))
	(setf (gl:glaref arr i) (aref data i)))
      (gl:buffer-data :array-buffer :dynamic-draw arr))
    (gl:bind-buffer :array-buffer 0)
    (make-vbo :dim (second shape) :length (first shape) :ref ref)))

(defun delete-vbo (matrix vbo)
  (gl:delete-buffers (list (vbo-ref vbo))))
(defun hash-vbo (matrix) matrix)

(defvar *vbo-manager* (item-manager :max-items 20 :load-fcn #'load-vbo :delete #'delete-vbo :hash #'hash-vbo))
(defun draw-vbos (draw-kind &rest vbos))
(defmethod load-graph-elem ((poly polygon) context)
  (

(defmethod load-graph-elem ((buf buffer) context)
  (cons (cons 'buffer (cons buf (get-assoc 'buffer context))) 
	context))

(defun bind-vbo (vbo loc)
  (with-slots (ref dim) vbo
    (gl:bind-buffer :array-buffer ref)
    (gl:enable-vertex-attrib-array 0)
    (gl:vertex-attrib-pointer loc dim :float nil 0 (cffi:null-pointer))))

(defun draw-polygon (draw-kind &rest vbos)
  (loop 
     for vbo in vbos
     for x from 0 to 100 do
       (bind-vbo vbo x))
  (gl:draw-arrays draw-kind 0  (min (mapcar (lambda (vbo) (vbo-length vbo))))))

(defmethod load-graph-elem ((draw draw) context)
  (let ((buffers (cdr (assoc 'buffer context))))
    (flet ((find-buffer (id)
	     (find-if (alexandria:curry #'eq id) buffers :key #'buffer-program-identifier)))
      (draw-polygon (draw-kind draw) (find-buffer 0) (find-buffer 1))
      )))

(defun shader (vertex-code fragment-code)
  (make-shader :vertex-code vertex-code
	       :fragment-code fragment-code))

(defstruct window
  (size (m:vec 1 1))
  (title "untitled")
  (win-id (gensym)))

(defun create-window(&key (size (m:vec 1 1)) 
		       (title "untitled") 
		       (id (gensym))
		       (start-location nil))

  (make-window :size size :title title :win-id id))

(defclass window-holder ()
  ((window :initarg :win)
   (id :initarg :id)
   (last-check :initform (utils:accurate-time))
   (time-out :initform 1.0 :initarg :time-out)
   (glfw-ref :initarg :glfw-ref)))

(defclass context-manager ()
  ((windows :initform (make-hash-table))))

(defvar *dispatch* nil)
(defvar *context-manager* (make-instance 'context-manager))
(defun destroy-window (win-holder context-manager)
  (with-slots (glfw-ref id) win-holder 
    (with-slots (windows) context-manager
      (glfw:destroy-window (print glfw-ref))
      (remhash id windows ))))

(defun update-window-timeouts (context-manager)
  (with-slots (windows) context-manager
    (let ((now (utils:accurate-time)))
      (with-package:with-package enumerate
	(for-each (alexandria:rcurry #'destroy-window context-manager)
	 (where 
		   (lambda (win)
		     (with-slots (last-check time-out) win
		       (> (- now last-check) time-out)))
		   (walk-list (alexandria:hash-table-values windows))
		   )))))
  (sleep 0.05)
  (dispatcher:begin-invoke *dispatch* (alexandria:curry #'update-window-timeouts context-manager)))

(defun create-window-holder (window-proto)
  (with-slots (size title win-id) window-proto
    (let ((win-ref (glfw:create-window (m:ref size 0) (m:ref size 1) 
				       title
				       (cffi:null-pointer) (cffi:null-pointer))))
      (let ((output (make-instance 'window-holder :win window-proto :id win-id
				   :time-out 1.0
				   :glfw-ref win-ref)))
	(glfw:show-window win-ref)
	output))))

(defun ensure-dispatch-loaded()
  (unless *dispatch*
    (setf *dispatch* (make-instance 'dispatcher:thread-dispatcher :name "graphics"))
    (dispatcher:begin-invoke *dispatch* (alexandria:curry #'update-window-timeouts *context-manager*))))

(defmethod get-window-holder ((context-manager context-manager) window)
  (with-slots (windows) context-manager
    (with-slots (win-id) window
      (let ((holder (gethash win-id windows)))
	(unless holder
	  (setf holder (create-window-holder window)))
	(setf (gethash win-id windows) holder)
	holder))))

(defun draw-scene-graph (scenegraph context)
  (if (typep scenegraph 'scene-graph)
      (let ((element (scene-graph-element scenegraph))
	    (child-elements (scene-graph-child-elements scenegraph)))
	(let ((outctx (load-graph-elem element context-copy)))
	  (loop for child-graph in child-elements do
	       (draw-scene-graph child-graph outctx))))
      (load-graph-elem scenegraph context)))
    
(defvar *initialized* nil)
(defun do-render-ui (window scene-graph timeout context-manager)
  (unless *initialized*
    (setf *initialized* t)
    (glfw:init))
  (restart-case
      (let ((win (get-window-holder context-manager window)))
	(with-slots (time-out glfw-ref last-check) win
	  (setf time-out timeout)
	  (setf last-check (utils:accurate-time))
	  (glfw:make-context-current glfw-ref)
	  (with-slots (size) window
	    (glfw:set-window-size glfw-ref (m:ref size 0) (m:ref size 1))
	    (glfw:show-window glfw-ref))
	  (let ((context (make-context)))
	    (draw-scene-graph scene-graph context))
	  (glfw:swap-buffers glfw-ref)
      
	  ))
    (skip ())
    ))
  

(defun render-ui (window scene-graph &key (timeout 1.0))
  (ensure-dispatch-loaded)
  (dispatcher:invoke *dispatch* 
		     (alexandria:curry #'do-render-ui window scene-graph 
			    timeout *context-manager*))
  )

(defun example-of-use ()
    (defvar scene
      (scene-node 
       (camera)
       (scene-node (material :texture "test.png" :color (m:vec 1.0 0.0 1.0 1.0))
		   (scene-node (camera :size (m:vec 100 100))
			       (uniform 'uv-scale 1.0 1.0)))))



    (defvar winid 'asd)
    (defvar window (create-window :size (m:vec 100 100) :title "window final" :id winid))
    (render-ui window scene :timeout 5.0))

(defun state-less-audio ()
  (let ((osc-node (osc :freq 440)))
    (stream-audio (node (output) osc osc))))

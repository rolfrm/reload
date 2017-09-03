(defpackage :functional-gui-items
  (:use :cl))
(in-package :functional-gui-items)

(defclass resource-managers ()
  (shader :initform (make-instance 'item-manager :max-items 5 :load-fcn #'load-shader
					   :delete #'delete-shader
					   :hash #'hash-shader))
  (texture :initform (make-instance 'item-manager :max-items 10 :load-fcn #'load-texture
				    :delete #'delete-texture))
  (vbo :initform (make-instance 'item-manager :max-items 10 :load-fcn #'load-vbo
				:delete #'delete-vbo 
				:hash #'hash-vbo)))


;;*** Leaf renderable ***;;;
(defclass leaf-renderable ())
(defmethod funtree:load-element ((leaf leaf-renderable) context child-elements)
  (unless child-elements
    (render-context context)))

(defun render-context (context)
  ;; (utils:let-list 
  ;;  (buffers draw) (utils:get-assocs context 'buffer 'draw)
  ;;  (flet ((find-buffer (id)
  ;; 	    (find-if (alexandria:curry #'eq id) 
  ;; 		     buffers 
  ;; 		     :key #'buffer-program-identifier)))
  ;;    (draw-polygon draw (find-buffer 0) (find-buffer 1))
  ;;    ))


  (print "rendering"))

;;** Transform **;
(defclass transform (leaf-renderable)
    ((rotation :initform 0.0)
     (translation :initform (m:vec 0.0 0.0))
     (scale :initform (m:vec 1.0 1.0))))

(defun transform (&key 
		    (rotation (m:vec 0.0 0.0))
		    (scale (m:vec 1.0 1.0))
		    (translation (m:vec 0.0 0.0)))
  (make-instance 'transform 
		 :rotation rotation
		 :scale scale
		 :translation translation))

(defmethod funtree:modify-context ((tform transform) context)
  (utils:let-list 
   (t r s) (utils:get-assocs context 'transform 'rotation 'scale)
   (with-slots ((tt translation)
		(tr rotation)
		(ts scale)) tform
     (utils:add-assocs 
      'transform (m:+ t (m:* s (mex:rotate-vec tt r)))
      'rotation (+ r tr)
      'scale (+ s ts) context))))

;;** Polygon **;;
(defclass polygon (leaf-renderable)
  ((vert :initform (m:vec))
   (uv :initform (m:vec))
   (draw-kind :initform :quads)))

;;** Buffer **;;
(defclass buffer (leaf-renderable)
  (values (m:matrix))
  (program-indentifier 0))

(defun buffer (matrix &key (shader-identifier 0))
  (make-buffer :values matrix
	       :program-identifier shader-identifier))
   
;;** Uniform **;;
(defclass uniform (leaf-renderable)
  ((name :initform "")
   (values :initform nil)))

(defun uniform (name &rest values)
  (make-uniform :name name :values values))

(defun set-context-uniform (uniform context))

(defmethod funtree:modify-context ((uni uniform) context)
  (set-context-uniform uni context))

;;** Draw **;;
(defclass draw (leaf-renderable)
  (draw-kind :quads))

(defun make-draw (&key (kind :quads))
  (make-draw :draw-kind kind))

;;;*** Material ***;;;
(defclass material (leaf-renderable)
  ((texture :initform nil :initarg :texture)
   (color :initform (m:vec 1.0 1.0 1.0 1.0) :initarg :color)))

(defun material (&key (texture nil) (color (m:vec 1.0 1.0 1.0 1.0)))
  (make-instance 'material :texture texture :color color))

(defmethod funtree:modify-context((mat material) context)
  (add-assoc 'render-target mat (add-assoc 'material mat context)))


;;*** Camera ***::
(defun camera (&key (size (m:vec  1 1)))
  "Width and height of nil means same as window"
  (make-instance 'camera :size size))

(defclass camera ()
  ((size :initform (m:vec 1 1) :initarg :size)))

;;***********Shader***********;;

(defclass shader(leaf-renderable)
  ((vertex-code :initform "")
   (fragment-code :initform "")))

(defmethod funtree:modify-context ((shader shader) context)
  (add-assoc 'shader shader context))

(defun shader (vertex-code fragment-code)
  (make-shader :vertex-code vertex-code
	       :fragment-code fragment-code))

(defclass shader-prog ()
  ((gl-ref :initform 0)))

(defun compile-shader-code (kind shader-str)
  (let ((shader-part (gl:create-shader kind)))
    (gl:shader-source shader-part shader-str)
    (gl:compile-shader shader-part)
    shader-part))

(defun shader-from-components(frag-shader vert-shader)
  (let ((program (gl:create-program)))
    (gl:attach-shader program vert-shader)
    (gl:attach-shader program frag-shader)
    (gl:link-program program)))
  
(defun shader-from-component-code (fragment-code vertex-code)
  (let ((frag-shader (compile-shader-code fragment :fragment))
	(vert-shader (compile-shader-code vertex :vertex)))
    (let ((vc-log (gl:get-shader-info-log vert-shader))
	  (fc-log (gl:get-shader-info-log frag-shader)))
	  (unless (equal vc-log "")
	    (print vc-log))
	  (unless (equal fc-log "")
	    (print fc-log)))
    (let ((shader (shader-from-components vc fc)))
      (gl:delete-shader vert-shader)
      (gl:delete-shader frag-shader)
      (make-shader-prog :gl-ref shader)
      )))
    
(defun load-shader(shader-proto)
  (let ((v (shader-vertex-code shader-proto))
	(f (shader-fragment-code shader-proto)))
    (shader-from-component-code v f)
    ))

(defun delete-shader (shader-proto shader)
  (let ((program (shader-prog-gl-ref shader)))
    (unless (eq program 0)
      (gl:delete-program program))))
  
(defun hash-shader (shader-proto)
  (concatenate 'string 
	       (shader-fragment-code shader-proto) 
	       (shader-vertex-code shader-proto)))

;; ** end of shader ** ;;

(defun load-texture(path))
(defun delete-texture(path tex))

(defmethod funtree:modify-context((cam camera) context)
  (let ((render-target (get-assoc 'render-target context))
	(current-context (get-assoc 'context context)))
    (unless (gethash current-context *resource-manager*)
      (setf (gethash current-context *resource-manager*)
	    (make-instance 'resource-managers)))
    ))


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

(defun draw-vbos (draw-kind &rest vbos))
(defmethod load-graph-elem ((poly polygon) context)
  ())

(defmethod load-graph-elem ((buf buffer) context)
  (utils:add-assoc 'buffer (cons buf (utils:get-assoc 'buffer context))))

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
  (gl:draw-arrays draw-kind 0  
		  (min (mapcar (lambda (vbo) (vbo-length vbo))))))

(defmethod load-graph-elem ((draw draw) context)
  (utils:add-assoc 'draw (draw-kind draw) context))

(defpackage :vbo
  (:use :cl :context-manager :window)
  (:export :gpu-array :gpu-array-length :bind-array
	   :set-vertexes :dimension))
(in-package :vbo)

(defclass vbo ()
  ((vertexes :initarg :vertexes :writer set-vbo-vertexes)
   (dim :initarg :dim :writer set-vbo-dim)
   (reference :initform nil :initarg ref)
   (vbo-profile :initform :dynamic-draw)
   (vbo-size :initform nil)))

(utils:export-class 'vbo)

(defmethod load-vbo ((bufferobject vbo) loc)
  (with-slots (reference dim) bufferobject
    (gl:bind-buffer :array-buffer reference)
    (gl:enable-vertex-attrib-array 0)
    (gl:vertex-attrib-pointer loc dim :float nil 0 (cffi:null-pointer))))

(defmethod delete-graphics-object ((self vbo))
  (with-slots (reference) self
    (gl:delete-buffers (list reference))
    (setf reference nil)))

(defmethod load-data ((bufferobject vbo))
  (with-slots (vertexes dim reference vbo-profile vbo-size) bufferobject
    (unless reference
      (setf reference (first (gl:gen-buffers 1))))
    (gl:bind-buffer :array-buffer reference)
    (gl:with-gl-array (arr :float :count (length vertexes))
      (dotimes (i (length vertexes))
	(setf (gl:glaref arr i) (nth i vertexes)))
      (if (equal vbo-size (length vertexes))
	  (gl:buffer-sub-data :array-buffer arr :offset 0 :buffer-offset 0
			      :size (* 4 (length vertexes)))
	  (gl:buffer-data :array-buffer vbo-profile arr))
      (setf vbo-size (length vertexes)))
    (gl:bind-buffer :array-buffer 0)))

(defclass gpu-array (g-object)
  ((vertexes :initform ())
   (dimension :initform 2)))

(defmethod gpu-array-length ((array gpu-array))
    (with-slots (vertexes dimension) array
      (if (and vertexes (not (eq dimension 0)))
	  (/ (length vertexes) dimension)
	  0)))

(def-g-object-setter gpu-array vertexes value 
  (unless (listp value) (error "only takes list..")))

(def-g-object-setter gpu-array dimension value 
  (unless (and (> value 0) (<= value 4)) (error "dimension")))

(defmethod bind-array ((self gpu-array) window loc)
  (let ((g-obj (load-from-context window self)))
    (load-vbo g-obj loc)))

(defmethod load-graphics-object ((self gpu-array) last-version)
  (with-slots (vertexes dimension) self   
    (progn
      (unless last-version
	(setf last-version (make-instance 'vbo :vertexes vertexes :dim dimension)))
      (setf (slot-value last-version 'vertexes) vertexes)
      (load-data last-version)
      last-version)))

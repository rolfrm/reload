(defpackage :shader-program (:use :cl :context-manager :window)
  (:export :gpu-program :gpu-program-object :set-shader-code :get-shader-code :bind-program :set-uniform
	   :shader-program)
  )
(in-package :shader-program)

(defclass shader-program ()
  ((shader-program :initform nil)
   (uniforms :initform (make-hash-table :test 'equal)) ;uniform to active-value translation
   (fragment-shader-code :initarg :fragment-code) 
   (vertex-shader-code :initarg :vertex-code)))

(defmethod bind-shader ((self shader-program))
  (with-slots (shader-program) self
    (when shader-program
	(gl:use-program shader-program))))

(defun _load-matrixes (matrixes program loc)
  (let ((dim (length (first matrixes))))
    (error "not supported")))

(defun _load-vectors (vectors program loc)
  (error "Not supported"))

(defun _load_uniform_value(values program loc)
  (if (every #'integerp values)
      (apply #'gl:uniformi loc values)
      (progn
	(apply #'gl:uniformf loc values)
	1)))

(defun get-uniform-loc (shader-prog uniform-name)
  (gl:get-uniform-location shader-prog uniform-name))

(defun compile-shader-code (kind shader-str)
  (let ((shader-part (gl:create-shader kind)))
    (gl:shader-source shader-part shader-str)
    (gl:compile-shader shader-part)
    shader-part))

(defmethod load-shader ((program shader-program))
  (with-slots (fragment-shader-code vertex-shader-code shader-program uniforms) program
    (if (not (and vertex-shader-code fragment-shader-code))
	(progn
	  (print "loading empty shader")
	  (setf shader-program 0))
	(let ((vc (compile-shader-code 
		   :vertex-shader vertex-shader-code))
	      (fc (compile-shader-code  
		   :fragment-shader fragment-shader-code)))
	  (print "loading new shader")
	  (let ((new-program (gl:create-program)))
	    (gl:attach-shader new-program vc)
	(gl:attach-shader new-program fc)
	(let ((vc-log (gl:get-shader-info-log vc))
	      (fc-log (gl:get-shader-info-log fc)))
	  (unless (equal vc-log "")
	    (print vc-log))
	  (unless (equal fc-log "")
	    (print fc-log)))
	(gl:link-program new-program)
	(gl:delete-shader vc)
	(gl:delete-shader fc)
	(let ((compile-status (gl:get-program new-program :link-status)))
	  (when compile-status)
	  (progn 
	    (when shader-program
	      (gl:delete-program shader-program))
	    (setf shader-program new-program))
	  (setf uniforms (make-hash-table :test 'equal))
	  program))))))

(defmethod update-uniform-values ((self shader-program) uniform-value-table)
  (with-slots (uniforms shader-program) self
    (loop for k being the hash-keys of uniform-value-table
       using (hash-value new-value) do
	 (let ((current-value (gethash k uniforms)))
	   (unless (equal new-value current-value) ;;if equal no reason to set
	     (let ((loc (get-uniform-loc shader-program k)))
	       (setf (gethash k uniforms) new-value)
	       (if (numberp (first new-value))
		   (_load_uniform_value new-value shader-program loc)
		   (if (vectorp (first new-value))
		       (if (numberp (aref (first new-value) 0))
			   (_load-vectors new-value loc shader-program)
			   (_load-matrixes new-value loc shader-program))))))))))

(defclass gpu-program-object (g-object)
  ;RAM side shader program
  ((shader-code :initform nil :writer set-shader-code :reader  get-shader-code)
   (kind :initform (error "Must supply shader kind") :initarg :kind)))

(def-g-object-setter gpu-program-object shader-code)
(def-g-object-setter gpu-program-object kind)

(defclass gpu-program (g-object)
 ((vertex-shader :reader get-vertex-shader  :initarg :vertex) 
  (fragment-shader :reader get-fragment-shader :initarg :fragment)
  (uniform-values :initform (make-hash-table :test 'equal))))

(defmethod set-uniform ((program gpu-program) uniform-name &rest args)
  (with-slots (uniform-values) program
    (setf (gethash uniform-name uniform-values) args)))

(defmethod get-revision ((self gpu-program))
  (with-slots (vertex-shader fragment-shader change-rev) self
    (+ change-rev
       (get-revision vertex-shader) 
       (get-revision fragment-shader))))

(def-g-object-setter gpu-program vertex-shader)
(def-g-object-setter gpu-program fragment-shader)

(defmethod bind-program ((self gpu-program) window)
  (let ((p-obj (load-from-context window self)))
    (with-slots (uniform-values) self
      (bind-shader p-obj)
      (update-uniform-values p-obj uniform-values))
    p-obj))

(defmethod update-program ((self gpu-program) window)
  (let ((p-obj (load-from-context window self)))
    (with-slots (uniform-values) self
      (update-uniform-values p-obj uniform-values)
      p-obj)))

(defmethod load-graphics-object ((self gpu-program) last-version)
  (with-slots (fragment-shader vertex-shader) self
    (progn
      (print "loading new program..")
      (let ((c-version last-version) 
	    (frag-code (get-shader-code fragment-shader))
	    (vert-code (get-shader-code vertex-shader)))
	(unless c-version
	  (setf c-version (make-instance 'shader-program)))
	(with-slots (fragment-shader-code vertex-shader-code) c-version
	  (setf fragment-shader-code frag-code)
	  (setf vertex-shader-code vert-code))
	(load-shader c-version)
	c-version))))

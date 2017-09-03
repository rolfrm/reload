
(defclass vbo ()
  ((vertexes :initarg :vertexes)
   (dim :initarg :dim)
   (changed :initform nil :initarg changed)
   (reference-value :initform nil :initarg ref)))

(defmethod load-data ((bufferobject vbo))
  (with-slots (vertexes dim changed reference) bufferobject
    (when changed
      (setf reference (first (gl:gen-buffers 1)))
      (gl:bind-buffer :array-buffer reference)
      (gl:with-gl-array 
	  arr
	:float
	(length vertexes)
      
	(dotimes (i (length vertexes))
	  (setf (gl:glaref arr i) (nth i vertexes)))
	(gl:buffer-data :array-buffer :static-draw arr))
      (gl:bind-buffer :array-buffer 0)))

(defmethod load-vbo ((bufferobject vbo) loc) nil)

(defclass shader-object ()
  ((reference :initform 0)
   (changed :initform nil)
   (kind :initform (error "shader-object: no kind supplied") 
	 :initarg :shader-kind)))

(defgeneric get-shader-source () (shader-object)
  (:documentation "Shader objects needs code")
  ())

(defclass uniform-object ()
    ((active :initform nil)
     (value :initform 0 )))

(defmethod set-uniform-value :after (new-value (self uniform-object))
  (with-slots (value active) self
    (setf active nil)
    (setf value new-value)))

(defclass shader-program ()
  ((shader-program :initform nil)
   (fragment-shader :initarg :fragment) 
   (vertex-shader :initarg :vertex)
   (uniform-table :initform (make-hash-table :test 'equal))))

(defmethod load-shader ((program shader-program))
  (with-slots (fragment-shader vertex-shader shader-program uniform-table) program
    (progn 
      (when (or 
	     (eq shader-program 0)
	     (slot-value fragment-shader 'changed)
	     (slot-value vertex-shader 'changed))
	(let ((vc (compile-shader-code 
		   (get-shader-code vertex-shader :vertex-shader)))
	      (fc (compile-shader-code 
		   (get-shader-code fragment-shader :fragment-shader))))
	  (when shader-program
	    (gl:delete-program shader-program))
	  (setf shader-program (gl:create-program))
	  (gl:attach-shader shader-program vc)
	  (gl:attach-shader shader-program fc)
	  (gl:link-program shader-program)
	  (gl:delete-shader vc)
	  (gl:delete-shader fc)
	  (maphash 
	   (lambda (key value) (setf (uniform-status-active program) 0)) 
	   uniform-table)))
      (gl:use-program shader-program))))

(defmethod set-uniform 
    ((program shader-program) uniform-name &rest args)
  (with-slots (uniform-table) program
    (setf (gethash uniform-name uniform-table) 
	  (make-uniform-status :value args :active nil))))

(defun _load-matrixes (matrixes program loc)
  (let ((dim (length (first matrixes))))
    (error "not supported")))

(defun _load-vectors (vectors program loc)
  (error "Not supported"))

(defun _load_uniform_value(values program loc)
  (if (every #'integerp val)
      (apply #'gl:uniformi val)
      (apply #'gl:uniformf val)))

(defmethod load-uniforms ((program shader-program))
  (with-slots (uniform-table shader-program) program
    (maphash 
     (lambda (key value)
       (let ((is-active (slot-value value 'active)))
	 (unless is-active
	   (let ((val (slot-value value 'value)))
	     (if (number p (first val))
		 (_load_uniform_value val loc shader-program)
		 (if (vectorp (listp val)) ;true -> vector or matrix array
		     (if (numberp (aref (first val) 0)) ; ->vector array
			 (_load-vectors val loc shader-program)
			 (_load-matrixes val loc shader-program))))
	     (setf (slot-value value 'active) t)))))
     uniform-table)))
			     
		      
(defun compile-shader-code ((sobj shader-program) kind shader-str)
  (let ((shader-part (gl:create-shader kind)))
    (gl:shader-source shader-part shader-str)
    (gl:compile-shader shader-part)
    shader-part))

(defclass shader-file () )
(defclass shader-string () ) 

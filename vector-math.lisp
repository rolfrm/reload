(cl:delete-package 'vector-math)
(defpackage :vector-math
  (:use cl-core cl-seq cl-sym)
  (:nicknames :vm))

(in-package :vector-math)
(defvar version '(0 1))

(defun _concat-symbols(sym1 sym2)
  (intern (concatenate 'string (symbol-name sym1) (symbol-name sym2))))

(defclass generic-vector ()
  ())

(defmethod vector-data ((vec generic-vector))
  (error "not implemented"))

(defmethod ref ((vec generic-vector) idx)
  (aref (vector-data vec) idx))

(defmethod (setf ref) (value (vec generic-vector) idx)
  (setf (aref (vector-data vec) idx)))

(defmethod dimension ((vec generic-vector))
  (error "Not implemented"))

;(defmacro define-vector-op (op)
(defparameter defined-vector-types ())  

(let ((dim-names '(x y z u v w r t s)))
  (defmacro create-vector-type (type-name data-type dimensions)
    (let ((axis-names (loop for dim-name in dim-names 
			   for idx from 0 below dimensions collect dim-name)))    
      (push (list type-name data-type dimensions) defined-vector-types)
      `(progn
	 (defclass ,type-name (generic-vector)
	   ((value-data :initform (make-array ,dimensions :element-type ',data-type))))

	 (defun ,type-name ,axis-names 
	   (cl:make-instance ',type-name :initial-data (list ,@axis-names)))
	 (export ',type-name)
	 (defmethod cl:initialize-instance :after ((vec ,type-name) cl:&key initial-data)
		    (with-slots (value-data) vec
		      (when initial-data
			(loop for x in (map 'list (lambda (x) x) initial-data)
			     for i from 0 to 100 do
			     (setf (aref value-data i) x)))))

       (defmethod vector-data ((vec ,type-name))
	 (cl:slot-value vec 'value-data))

       (defmethod cl:print-object ((vec ,type-name) stream)
	 (cl:format stream "(~a)" (vector-data vec)))

       (defmethod dimension ((vec ,type-name))
	 ,dimensions)

       (defmethod vector-apply ((vec ,type-name) fcn)
	 (let ((out make-instance ',type-name))
	   (with-slots ((out-v value-data)) out
	     (with-slots (value-data) vec
	       (loop for x from 0 below ,dimensions do
		    (setf (aref out-v x)  (funcall fcn (aref value-data x))))))
	   out))

       ,@(loop for dim in dim-names 
	    for idx from 0 below dimensions append
	      (list
	       `(defmethod ,(_concat-symbols 'get- dim) ((vec ,type-name))
		  (with-slots (value-data) vec
		    (aref value-data ,idx)))
	       `(defmethod (setf ,(_concat-symbols 'get- dim))
		    ((new-value ,data-type) (vec ,type-name))
		  (with-slots (value-data) vec
		    (setf (aref value-data ,idx) new-value)))))
      
    ))))

;(defun vector-add (&rest args)
  


;;(x) => ((x type)) 
;;(&rest args) => ((x type) &rest ags)
;;(x y) => ((x type) (y type))
;;(x y &rest) => ((x type) (y type) &rest args)
(defun args-to-static-args (arg-list caller-type)
  (if (eql (first arg-list) 'cl:&rest)
      (list (list 'x caller-type) 'cl:&rest 'args)
      (let ((rest-met ()))
	(loop for arg in arg-list collect
	     (if rest-met
		 arg
		 (progn
		   (if (eql arg 'cl:&rest)
		     (progn
		       (setf rest-met cl:t)
		       'cl:&rest)
		     (list arg caller-type))))))))

(defun get-rest-args (arg-list)
  (let ((rest-met 0) (arg-name ()))
    (loop for x in arg-list do
	 (when (eql rest-met 1)
	   (cl:incf rest-met)
	   (setf arg-name x))
	 (when (eql x 'cl:&rest)
	   (cl:incf rest-met))
	 )
    arg-name))

(defun get-before-rest-args (arg-list)
  (let ((rest-met 0) (args ()))
    (loop for x in arg-list do
	 (when (eql 0 rest-met)
	     (when (eql rest-met 0)
	       (if (eql x 'cl:&rest)
		 (setf rest-met 1)
		 (push x args)))))
	       
    (cl:reverse args)))

(defmacro def-vector-fcn (fcn-name args code)
  (list* 'progn 
	 (loop for (name type dim) in defined-vector-types collect
	      (let ((new-args (args-to-static-args args name)))
		`(defmethod ,fcn-name ,new-args
		   (make-instance ',name :initial-data 
				  (map 'vector (lambda ,args ,code) 
				       ,@(loop for arg in args collect (list 'vector-data arg)))))))))

(defmacro def-vector-fcn2 (fcn-name args code)
    (list 'progn (list* 'progn 
	 (loop for (name type dim) in defined-vector-types collect
	      (let ((new-args (args-to-static-args args name))
		    (rest-arg (get-rest-args args)))
		`(defmethod ,fcn-name ,new-args
		   (cl:make-instance ',name 
				     :initial-data
				     ,(if rest-arg
					  `(cl:apply #'map 'vector (lambda ,args ,code) 
						     ,@(loop for arg in (get-before-rest-args args) collect (list 'vector-data arg)) (mapcar #'vector-data ,rest-arg) )
					  `(cl:map 'vector (lambda ,args ,code) 
						   ,@(loop for arg in (get-before-rest-args args) collect (list 'vector-data arg))))))))
	 )
	  `(export ',fcn-name)))

(defun vec-apply (kind fcn cl:&rest args)
  (cl:make-instance kind :initial-data (cl:apply #'cl:map 'vector fcn (mapcar #'vector-data args))))
		      
(create-vector-type vec2i integer 2)
(create-vector-type vec3i integer 3)
(create-vector-type vec3f single-float 3)
(create-vector-type vec2f single-float 2)

(def-vector-fcn2 add17 (x cl:&rest args) (cl:apply #'cl:+ x args))
(def-vector-fcn2 + (x cl:&rest args) (cl:apply #'cl:+ x args))
(def-vector-fcn2 - (x cl:&rest args) (cl:apply #'cl:- x args))
(def-vector-fcn2 * (x cl:&rest args) (cl:apply #'cl:* x args))
(def-vector-fcn2 / (x cl:&rest args) (cl:apply #'cl:/ x args))
(def-vector-fcn2 sin (x) (cl:sin x))
(def-vector-fcn2 cos (x) (cl:cos x))
(def-vector-fcn2 tan (x) (cl:tan x))
(def-vector-fcn2 floor (x) (cl:coerce (cl:floor x) 'single-float))

(export 'create-vector-type)
(export 'vector-data)
(export 'dimension)
(export 'get-x)
(export 'get-y)
(export 'get-z)
(export 'get-w)
(export 'def-vector-fcn)
(export 'def-vector-fcn2)
(export 'args-to-static-args)
(export 'vec3f)
(export 'vec3f)
(export 'get-rest-args)
(export 'get-before-rest-args)

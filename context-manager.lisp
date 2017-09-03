(defpackage :context-manager (:use :cl)
	    (:export :g-object :load-graphics-object
		     :def-g-object-setter :g-obj-rev-set
		     :increment-rev :context-manager
		     :insert-in-context-manager :get-revision
		     :set-revision :add-context
		     :set-internal-format
		     :load-graphics-object-to-context))
(in-package :context-manager)

(defvar __unique-key 0)

(defclass g-object ()
  ((change-rev :initform 0 
	       :reader get-revision 
	       :writer set-revision)
   (unique-id :initform (incf __unique-key) 
	      :accessor id)))

(utils:export-class 'g-object)

(defgeneric load-graphics-object (g-object last-version)
  (:documentation "how each opengl/graphics object loads itself")
  (:method ((gobj g-object) last-version) 
    (print "load-graphics-object undefined")))

(defmacro def-g-object-setter (class slot-name &optional (value-name 'value2) body)
  "When setting the value of a g-object the change revision should be increased"
  (let ((symname (alexandria:symbolicate 'set- slot-name)))
    `(defmethod ,symname ((self ,class) ,value-name)
       (with-slots (,slot-name change-rev) self
	 (progn
	   ,(when body
		  body)
	   (unless (equal ,value-name ,slot-name)
	     (incf (slot-value self 'change-rev))
	     (setf (slot-value self ',slot-name) ,value-name)))))))

(defmacro g-obj-rev-set (class value-name reader-name)
  `(defmethod ,reader-name :before ((self ,class) ,value-name)
	      (incf (slot-value self 'change-rev))))

(defmethod increment-rev ((self g-object))
  (with-slots (change-rev) self 
    (incf change-rev)))

(defclass context-manager ()
  ((current-context :initform (make-hash-table :test 'equal))
   (contexts :initform (list))))

(defun insert-in-context-manager (context-manager context-object)
  (with-slots (object-id) context-object
    (with-slots (current-context) context-manager
      (setf (gethash object-id current-context) context-object))))

(defun add-context (context-manager context-holder)
  (with-slots (contexts) context-manager
    (push context-holder contexts)))

(defclass context-object ()
  ((object-id :initform nil :initarg :id)
   (last-change-ref :initform nil)
   (gl-object :initform nil :initarg :object)))

(defmethod load-graphics-object-to-context ((context context-manager) 
					    (gobj g-object))
  (with-slots (unique-id) gobj
    (with-slots (current-context) context
      (let ((the-context-object (gethash unique-id current-context)))
	(unless the-context-object
	  (setf the-context-object 
		(insert-in-context-manager 
		 context 
		 (make-instance 'context-object :id unique-id))))
	(let ((change-rev (get-revision gobj)))
	  (with-slots (last-change-ref gl-object) the-context-object
	    (unless (equal last-change-ref change-rev)
	      (progn
		(setf (slot-value the-context-object 'last-change-ref) change-rev)
		(setf gl-object (load-graphics-object gobj gl-object))))
	    gl-object))))))

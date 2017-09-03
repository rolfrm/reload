

(defun combine-symbols (&rest args)
  (intern (apply #'concatenate 'string (mapcar #'symbol-name args)))) 

(defclass graph-object ()
  ((__bind :initform ())
   (__parent :initform ())
   (__root :initform ())))

(defmethod __parent-changed ((self graph-object) old-value new-value)
  ())

(defun raw-set-value (target slot &rest args)
  (if (equal (length args) 1)
      (setf (slot-value target slot) (first args))
      (apply #'raw-set-value (slot-value target slot) (first args) (rest args))))


(defun set-value (target slot &rest args)
  (let ((visited-nodes (list)))
    (labels ((+set-value 
	       (target slot &rest args)
	     (if (equal (length args) 1)
		 (unless (find `(,target ,slot) visited-nodes :test #'equal)
		   
		   (setf (slot-value target slot) (first args)) ;; First set value
		   (push `(,target ,slot) visited-nodes)
		   (funcall (combine-symbols slot '-changed) target (first args) 0)
		   (loop for bnd in (slot-value target '__bind) do ;;look for bindings
			(when (equal (first bnd) slot)
			  (apply #'+set-value target (append (second bnd) args)))))		 
		 (apply #'+set-value (slot-value target slot) (first args) (rest args)))))
      (apply #'+set-value target slot args))))


(defun get-value (target slot &rest slots)
    (if slots
	(apply #'get-value (slot-value target slot) (first slots) (rest slots))
	(slot-value target slot)))

(defun keywd-to-sym (keywd)
  (intern (symbol-name keywd)))

(defun sym-to-keywd (sym)
  (intern (format nil "~a" sym) "KEYWORD"))

(defmacro defgraph (symname properties &rest body)
  (let ((bindings (list))
	(current-symbol nil)
	(property-names (mapcar (lambda (x) (if (listp x) (first x) x)) properties))
	(property-list (mapcar (lambda (x) 
				 (if (listp x)
				     `(,(first x) :initform ,(second x))
				     x)) properties))
	(hidden-property-idx 0)
	(obj-relations `(progn))
	(root-bindings (list))
	(child-bindings (list))
	(value-sets `(progn))
	(override-property-values `(progn))
	(sub-graphs (list)))
    (loop for x in properties do
	 (when (listp x)
	   (push `(set-value self ',(first x) ,(second x)) override-property-values)))
    (labels ((handle-entry (x)
	       (cond 
		 ((not (listp x)) 
		  (if (find x property-names) (list :prop x)
		      (list :eval x)))
		 ((or (equal (first x) 'bind) (find (first x) properties)) (list :prop (second x)))
		 ((subtypep (first x) 'graph-object) (list :graph (unroll-subgraph :graph x)))
		 (t (list :eval x))))
	     
	     (+unroll-subgraph (state keywd value rest)
	       (progn
		 (unless (keywordp keywd)
		   (error "second argument not a keyword..."))
		 (let ((read-name (if (equal keywd :name) value nil)))
		   (if read-name
		       (if (> (length rest) 1)
			   (append (list read-name) (rest (+unroll-subgraph state (first rest) (second rest) (rest (rest rest)))))
			   read-name)
		       
		       (let ((read-value (append (handle-entry value) (list keywd))))
			 (if (> (length rest) 1)
			     (append 
			      (+unroll-subgraph state (first rest) (second rest) (rest (rest rest)))
			      (list read-value))
			     (list nil read-value)))))))

	       (unroll-subgraph (state _body)
		 (let* ((unrolled-graph 
			 (+unroll-subgraph state (second _body) 
					   (third _body) 
					   (rest (rest (rest _body)))))
			(name (first unrolled-graph))
			(kind (first _body))
			(props (rest unrolled-graph)))
		   (setf name 
			 (if name 
			     name 
			     (intern (format nil "_PROP~a" (incf hidden-property-idx)))))
		   (push `(,name :initform (make-instance ',kind)) property-list) 
		   (push `(raw-set-value self ',name '__root self) obj-relations)
		   (let ((own-bindings (list)))
		     (print props)
		     (loop for x in props do
			  (case (first x)
			    (:prop (let ((root-prop (second x)) (child-prop (keywd-to-sym (third x))))
				     (push `(,root-prop (,name ,child-prop)) root-bindings)
				     (push `(,child-prop (__root ,root-prop)) own-bindings)
				     (push `(set-value self ',root-prop (get-value self ',name ',child-prop)) value-sets)
				     ))
				     
			    (:eval (let ((child-prop (keywd-to-sym (third x))) (evaluation (second x)))
				     (print (format nil "EVAL: ~a" x))
				     (push `(set-value self ',name ',child-prop ,evaluation) value-sets)))
			    (:graph
			     (push `(set-value self ',name ',(keywd-to-sym (third x)) (get-value self ',(second x))) value-sets)
			     (push `(raw-set-value self ',(second x) '__parent (get-value self ',name)) obj-relations))))
		     (when (> (length own-bindings) 0)
		       (push `(,name ,own-bindings) child-bindings)
		       ))
			
		  
		   name))
			   
			   
	     (unroll-body (state _body)
	       (loop for x in _body do
		    (let* ((all (handle-entry x)) (type (first all)) (name (second all)))
		      (push `(raw-set-value self ',name '__parent self) obj-relations)))))
      (progn 
	(unroll-body :graph body)
	(setf child-bindings (append '(progn)
	      (mapcar 
	       (lambda (propgroup) 
		 `(raw-set-value self ',(first propgroup) '__bind (append ',(second propgroup) (get-value self ',(first propgroup) '__bind))))
	       child-bindings)))
	
	 `(progn 
	    (defclass ,symname (graph-object) 
	      ,(mapcar (lambda (x) (if (listp x) x `(,x :initform nil :initarg ,(sym-to-keywd x))))   property-list))
	    ,(append '(progn) (let ((bare-props (mapcar (lambda (x) (if (listp x) (first x) x)) property-list)))
		   (mapcar 
		    (lambda (x)
		      `(defmethod ,(combine-symbols x '-changed) ((self ,symname) new-value old-value)
			 ()))bare-props)))
	    (defmethod initialize-instance :after 
	      ((self ,symname) &key)
	      (progn 
		(raw-set-value self '__bind ',root-bindings)
		,(reverse obj-relations)
		,child-bindings
		,(reverse value-sets)
		,(reverse override-property-values)
		)))
	))))


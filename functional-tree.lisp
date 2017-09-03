(defpackage :functional-tree (:use :cl :reload-engine)
	    (:nicknames :funtree)
	    (:export :scene-node :node :load-element :traverse-child-elements :modify-context :render-tree)
	    )
(in-package :functional-tree)

(defstruct scene-graph 
  (element)
  (child-elements))

;;creates a node in a scene-graph. Can have node children
(defun scene-node (node-object &rest sub-nodes)
  (make-scene-graph :element node-object
		    :child-elements sub-nodes))
(defun node (node-object &rest sub-nodes)
  (apply #'scene-node node-object sub-nodes))

(defun traverse-child-elements (context child-elements)
  (remove-if 
   #'null 
   (loop for child in child-elements collect
	(render-tree child context nil))))

(defgeneric modify-context (element context)
  (:documentation "overload if a change to context is wanted")
  (:method (any-object context) context))

(defgeneric load-element (element context child-elements)
  (:documentation "called each time element is traversed. should return a modified context a-list")
  (:method (element context child-elements) 
    (remove-if 
     #'null 
     (loop for child in child-elements collect
	  (render-tree child context nil))))
  
  (:method ((scenegraph scene-graph) context child-elements)
    (let ((element (scene-graph-element scenegraph))
	  (child-elements (scene-graph-child-elements scenegraph)))
      (render-tree element context child-elements)))
      
  )

(defun render-tree (element &optional (context nil) (child-elements nil))
  (load-element element (modify-context element context) child-elements))


(defpackage :functional-tree-test (:use :cl :functional-tree))
(in-package :functional-tree-test)

(defclass lambda-based-node ()
  ((load-func :initform (lambda (context) context)
	      :initarg :load)))

(defmethod funtree:load-element :before ((lmb lambda-based-node) context child-elements)
  )

(defmethod funtree:modify-context ((lmb lambda-based-node) context)
  (with-slots(load-func) lmb
    (funcall load-func context)))

(defmethod funtree:load-element :around ((lmb lambda-based-node) context child-elements)
  (call-next-method))

(defmethod funtree:load-element ((lmb lambda-based-node) context child-elements)
  (call-next-method))

(defun lmb-based(lmb)
  (make-instance 'lambda-based-node :load lmb))

(test:clear-package-tests :functional-gui-test)

 (defun call-test()
   (let ((cnt 0))
     (flet ((count-up-and-move (context)
	      (incf cnt)
	      context))
       (flet ((counter-node () (lmb-based #'count-up-and-move)))
	 (let ((scene-graph (scene-node (counter-node)
					(counter-node) 
					(scene-node (counter-node)))))
	   (load-element scene-graph nil nil)
	   (unless (eq cnt 3)
	     (error (format nil "Unexpected count : ~a" cnt)))
	   (load-element scene-graph nil nil)
	   (unless (eq cnt 6)
	     (error (format nil "Unexpected count : ~a" cnt)))
	   )))))

(test:register 'call-test)

(defun context-test ()
  (let ((contexts nil))
    (flet ((node1 () (lmb-based 
		      (lambda (context) 
			(push context contexts)
			(let ((as1 (utils:get-assoc 'as1 context)))
			  (utils:add-assoc 'as1 (if as1 (+ 1 as1) 0) context)))))
	   (node2 () (lmb-based 
		      (lambda (context) 
			(let ((as2 (utils:get-assoc 'as2 context)))
			  (utils:add-assoc 'as2 (if as2 (- as2 1) 0) context)))))
	   )
      (let ((sg (scene-node (node1) (node2) 
			    (scene-node (node1) (node2) 
					(scene-node (node2) 
						    (scene-node (node1)))))))
	
	(load-element sg nil nil)
	(print contexts)
	(unless (eq (length contexts) 3)
	  (error "Not all nodes traversed"))
	(when (> (count nil contexts) 1)
	  (error "Unexpected saved contexts"))
	))))

(test:register 'context-test)

(defclass lambda-based-around ()
  ((load :initarg :load :initform (lambda (context child-elem) context))
   (unload :initarg :unload :initform (lambda (context child-elem) context))))

(defmethod funtree:load-element :around ((lmb lambda-based-around) context child-elements)
  (with-slots(load unload) lmb
    (let ((new-context (funcall load context child-elements)))
      (let ((outp (call-next-method lmb new-context child-elements))); lmb (funcall load context child-elements) child-elements)
	(funcall unload new-context child-elements)
	outp))))

(defun lmb-around(load unload)
  (make-instance 'lambda-based-around :load load :unload unload))

(defun around()
  (let ((calls 0)
	(leaf-nodes 0))
    (flet ((node1 () 
	     (lmb-around 
	      (lambda (context child-elements) 
		(incf calls)
		(unless child-elements
		  (incf leaf-nodes))
		(unless (eq 2 (length context))
		  (error "Unexpected context length"))
		context)
	      (lambda (context child-elements)
		(unless (eq 2 (length context))
		  (error "Unexpected context length"))))))
      
      (let ((sg (scene-node (node1) (node1) 
			    (scene-node (node1) (node1) 
					(scene-node (node1) 
						    (scene-node (node1)))))))
	(load-element sg '(1 2) nil)))
    (unless (eq calls 6)
      (error (format nil "Unexpected number of calls. Expected 6 got ~a" calls)))
    (unless (eq leaf-nodes 3)
      (error (format nil "Unexpected number of leaf nodes. Expected 3 got ~a" leaf-nodes)))
    
    ))
(test:register 'around)

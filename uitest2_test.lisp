(defgraph d (a))

(defmethod a-changed ((self d) new-value old-value)
  (print (format nil "----> ~a ~a" new-value old-value)))

(defgraph e ((b 5))
  (d :a b))

(defgraph f (a)
  (e :b a))

(defgraph g ((a 123))
  (f :name f2 :a a)
  (f :name f1 :a a))

(defmethod draw ((self g))
  ())

(defparameter graph1 (make-instance 'g))

(defun print-graph (graph)
  (let ((bindings (get-value graph '__bind)))
    (print bindings)
    (loop for x in bindings do
	 (let ((child-name (if (listp (second x))
			       (first (second x))
			       (second x))))
	   (let ((child-value (get-value graph child-name)))
	     (if (not (or (equal child-name '__root) (equal child-name '__parent)))
		 (if (subtypep (type-of child-value) 'graph-object)
		     (progn
		       (print child-name)
		       (print-graph child-value))
		     (print child-name))
		   (print (format nil ": ~a" (first x)))
		   ))))))
(defun any-equal-to-first (first second &rest values)
  (if (eq first second)
      t
      (if (not values)
	  nil
	  (apply #'any-equal-to-first first values))))
	  
  
(defmethod +print (anyobject name)
  (list (format nil "~a: ~a" name anyobject)))

(defun get-filtered-slots (graph)
  (remove-if 
   (lambda (slot-name) (any-equal-to-first slot-name '__root '__parent '__bind))
   (class-slot-list (class-of graph))))


(defmethod +print ((graph-special graph-object) name)
  (list 
   (format nil "~a ~a:" name (type-of graph-special))
   (mapcar 
    (lambda (slot-name)
      (+print (get-value graph-special slot-name) slot-name))
    (get-filtered-slots graph-special)
    )))
   
(defmethod +print ((any-class standard-object) name)
  (list (format nil "object ~a: ~a" name any-class)))

(defun print-graph (graph-object name)
  (labels ((unfold (gexp level)
	     ;(print gexp)
	     (if (listp gexp)
		 (progn 
		   (print (format nil "~a- ~a" (make-string (* level 3) :initial-element #\space) (first gexp)))
;		   (print (format "- ~a" (first gexp)))
		   (loop for x in (second gexp) do
			(unfold x (+ 1 level) )))
		 (print gexp))))
    (progn 

      (print "**** GRAPH EXPANSION ****")
      (unfold (+print graph-object name) 0))))

(defun get-slots (obj)
  (class-slot-list (class-of obj)))

(defun get-slot-info (obj)
  (let ((slots (get-slots obj)))
    (mapcar 
     (lambda (slot) 
       (let ((slot-value (get-value obj slot)))
	 (list slot slot-value (type-of slot-value))))
     slots)))

(defun slot-info-name (slot-info) (first slot-info))
(defun slot-info-value (slot-info) (second slot-info))
(defun slot-info-type (slot-info) (third slot-info))

(defun typename-of (object)
  (intern (symbol-name (type-of object))))

;; update graph to become similar to target graph.
;; Special for graphs. Easier for simper objects
;; Graphs: 
(defun update-graph (graph target-graph)
  (let ((orig-slots (get-slot-info graph))
	(target-slots (get-slot-info target-graph)))
    (loop for x in target-slots do
	 (print (slot-info-name x)))))


;;package test
(export '(defgraph update-graph get-value set-value))
(defvar it 0)
(defparameter pkname (intern (format nil "p~a" (incf it))))
(defpackage pkname)

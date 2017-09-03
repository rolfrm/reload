;;package test
(defmethod pt2 (any)
  (print "..."))
(export '(defgraph pt2 update-graph get-value set-value slot-value class-slot-list))
(defvar it 0)

(defparameter pkname (intern (format nil "P~a" (incf it))))
(eval `(defpackage ,pkname))
(eval `(cl:in-package ,pkname))
(cl:use-package :cl-user)
(cl:print "l")

(cl-user:defgraph A ((a 5)))

(cl:defmethod cl-user:pt2 ((self A))
  (cl:print "........1111"))

(cl:print (cl:describe 'A))
(cl:print "o")
(cl:defvar local-graph (cl:make-instance 'A))
(cl:export 'local-graph)
(cl:in-package :cl-user)

(cl:print "got here..")
(defparameter pkname2 (intern (format nil "P~a" (incf it))))
(eval `(defpackage ,pkname2))
(eval `(cl:in-package ,pkname2))
(cl:use-package :cl-user)
(cl-user:defgraph A (a b c))
(cl:defmethod pt2 ((self A))
  (cl:print "123123"))
(cl:defvar local-graph (cl:make-instance 'A))
(pt2 local-graph)
(cl:export 'local-graph)
(cl:in-package :cl-user)

(print "updating...")
(update-graph (symbol-value (print (intern "LOCAL-GRAPH" pkname))) 
	      (symbol-value (print (intern "LOCAL-GRAPH" pkname2))))
(update-graph (symbol-value (print (intern "LOCAL-GRAPH" pkname2))) 
	      (symbol-value (print (intern "LOCAL-GRAPH" pkname))))
(defparameter asd (symbol-value (intern "LOCAL-GRAPH" pkname)))
(defparameter asd2 (symbol-value (intern "LOCAL-GRAPH" pkname2)))

(delete-package pkname)
(delete-package pkname2)

(update-graph asd asd2)
(update-graph asd2 asd)

(pt2 asd2)
(pt2 asd)

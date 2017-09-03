(defclass go-task()
  ((validator :initarg :valid)
   (task-execute :initarg :exec)
   (cost-calculation :initarg :cost)))


(defclass graph-occupier ()
  ((graphical-model :initform nil :initarg :model)
   (offset :initform (matrix:make-vector '(0.0 0.0 0.0)) :initarg :offset)
   (current-vertex :initform nil :accessor occupant-vertex)
   (tasks :initform (make-queue))))

(defclass world-object (graph-occupier)
  ((offset :initform (matrix:make-vector '(0.0 0.0 0.0)) :initarg :offset)
   (name :initform "" :initarg :name)
   ))

(defclass collectable (world-object) ())

(re.load "graph/entity.lisp")

(defmethod enqueue-task ((graph-occupier graph-occupier) (task go-task))
  (with-slots (tasks) graph-occupier
    (enq tasks task))) 


(defmethod dequeue-task ((graph-occupier graph-occupier))
  (with-slots (tasks) graph-occupier
    (when (queued? tasks)
      (funcall (deq tasks)))))

(defmethod any-tasks ((graph-occupier graph-occupier))
  (with-slots (tasks) graph-occupier
    (queued? tasks)))

(defmethod next-task-cost ((graph-occupier graph-occupier))
  (with-slots (tasks) graph-occupier
    (cost-task (peek tasks))))

(defmethod task-exec ((graph-occupier graph-occupier))
  (with-slots (tasks action-points action-points-threshold) graph-occupier
    (when (any-tasks graph-occupier)
      (let ((task-cost (next-task-cost graph-occupier)))
	(when (< task-cost action-points)
	  (let ((task (deq tasks)))
	    (when  (validate-task task)
	       (decf action-points task-cost)
	       (execute-task task)))
	   (task-exec graph-occupier))))))

;(defmethod do-update-graph-occupier :after ((graph-occupier graph-occupier) vertex)
;     (with-slots (action-points) graph-occupier
;       (incf action-points 0.05)))

 (defmethod do-update-graph-occupier ((graph-occupier graph-occupier))
   (update-graph-occupier graph-occupier)
   (task-exec graph-occupier))

 (defmethod update-graph-occupier ((graph-occupier graph-occupier))
   ())


(defmethod validate-task ((task go-task))
  (with-slots (validator) task
    (funcall validator)))

(defmethod execute-task ((task go-task))
  (with-slots (task-execute) task
    (funcall task-execute)))

(defmethod cost-task ((task go-task))
  (with-slots (cost-calculation) task
    (funcall cost-calculation)))

(defmacro deftask (name args &key (valid ()) (exec ()) (cost ()))
  `(defun ,name ,args
     (make-instance 'go-task
		    :valid (lambda () ,valid)
		    :exec (lambda () ,exec)
		    :cost (lambda () ,cost))))

(defun move-occupier (obj start-vert stop-vert)
  (progn
    (with-slots (occupiers) start-vert
      (setf occupiers (remove obj  occupiers)))
    (with-slots (occupiers) stop-vert
      (setf occupiers (cons obj occupiers)))
    (setf (occupant-vertex obj)stop-vert)))

;;requires cl-cont
(cl-cont:defun/cc yeild ()
    (cl-cont:let/cc
	k
      k))

(cl-cont:defun/cc yield ()
    (cl-cont:let/cc
	k
      k))

       

(defclass continuum ()
  ((continuation :initarg :continuum))
  (:documentation "A the continued calling of a continuation"))

(defmethod continuum-step ((cnt continuum))
  (with-slots (continuation) cnt
      (setf continuation (funcall continuation))))

(defclass game-object-ai()
  ;Active behaviours. Multiple pseudo threads. Can cause interruptions
  ;on the main behaviour
  ((active-behaviours :initform nil)
   (to-delete :initform nil)
   (world :initform nil)))

(defmethod get-world ((game-object-ai game-object-ai))
  (slot-value game-object-ai 'world))

(defmethod set-world ((game-object-ai game-object-ai) world-value)
  (setf (slot-value game-object-ai 'world) world-value))

(defmethod step-behaviours ((game-object-ai game-object-ai) _world)
  (with-slots (active-behaviours to-delete world) game-object-ai
    (setf world _world)
    (loop for cc in active-behaviours do
	 (progn
	   (unless (find cc to-delete) ;;Dont use to be deleted..
	     (continuum-step cc))
	   cc))
    (setf active-behaviours (remove-if (rcurry #'find to-delete) active-behaviours))
    (setf to-delete nil)))

(defmethod delete-behaviour ((game-object-ai game-object-ai) (behaviour continuum))
  (with-slots (to-delete active-behaviours) game-object-ai
    (push behaviour to-delete)))

(defmethod add-behaviour ((game-object-ai game-object-ai) (behaviour continuum))
  (with-slots (active-behaviours) game-object-ai
    (push behaviour active-behaviours)))

(defmethod add-behaviour ((game-object-ai game-object-ai) (behaviour function))
  (with-slots (active-behaviours) game-object-ai
    (push (make-instance 'continuum :continuum behaviour) active-behaviours)))

(defmethod add-behaviour ((game-object-ai game-object-ai) anyth)
    (error "behaviour must be function or continuum"))

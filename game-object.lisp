(defclass game-object () 
  ((location :initform #(0 0 0) :initarg :location)
   (physics :initform nil :initarg :physics)))

(defmethod game-step ((go game-object))
  ())

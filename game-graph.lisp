(defpackage :game-graph
  (:use :cl :graph)
  (:export :game-vertex :location :terrain-type :occlusion :occupiers :models :size
	   :game-edge :width ))
(in-package :game-graph)

(defclass game-vertex (vertex)
  ((location :initarg :location )
   terrain-type
   occlusion
   (occupiers :initform nil)
   models
   (size :initarg :size)))

(defun vertex-location (game-vert)
  (with-slots(location) game-vert
    location))

(defclass game-edge (edge)
  ((width :initarg :width) movement-cost terrain-type models))

(defmethod on-split ((game-vertex game-vertex) graph)
  (let ((vertexes (get-connected-vertexes game-vertex)))
    (let* ((mid-pt 
	    (matrix:scale 
	     (apply #'m:+ (mapcar #'vertex-location vertexes))
	     0.5)))
      (with-slots(location) game-vertex
	(setf location mid-pt)))))

(defun test-game-graph ())

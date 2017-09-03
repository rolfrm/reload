(defpackage :graph
  (:use :cl)
  (:export :vertex :edges :edge :vertex-1 :vertex-2 :graph :vertexes :vertex-kind :edge-kind :get-cost
	   :on-split :get-vertexes :make-vertex :connect-vertexes :split-edge :get-edge))
(in-package :graph)

(defclass vertex ()
  ((edges :initform '())))

(defclass edge ()
  ((vertex-1 :initarg :vert1)
   (vertex-2 :initarg :vert2)
   ))

(defclass graph ()
  ((vertexes :initform nil)
   (edges :initform nil)
   (vertex-kind :initform 'vertex :initarg :vertex-kind)
   (edge-kind :initform 'edge :initarg :edge-kind)))

(defmethod get-cost((edge edge))
  1.0)

(defmethod on-split ((vertex vertex) graph))

(defun get-edge-vertexes(edge)
  (with-slots (vertex-1 vertex-2) edge
    (list vertex-1 vertex-2)))

(defun get-graph-vertexes (graph)
  (with-slots(vertexes) graph
    vertexes))

(defun make-graph-vertex (graph)
  (with-slots(vertex-kind) graph
    (make-instance vertex-kind)))

(defmethod make-graph-edge ((graph graph) vert1 vert2)
  (with-slots (edge-kind) graph
    (make-instance edge-kind
	   :vert1 vert1 
	   :vert2 vert2)))

(defmethod insert-vertex ((graph graph) vert)
  (with-slots (vertexes) graph
    (push vert vertexes)
    vert))

(defmethod connect ((graph graph) vert1 vert2 &rest edge-annotations)
  (with-slots (edges) graph
    (let ((new-edge 
	   (apply #'make-graph-edge graph vert1 vert2 edge-annotations)))
      (push new-edge edges)
      (with-slots (edges) vert1
	(push new-edge edges))
      (with-slots (edges) vert2
	(push new-edge edges))
      new-edge)))

(defun make-graph (&optional (vertex-kind 'vertex) (edge-kind 'edge))
  (make-instance 'graph 
		 :vertex-kind vertex-kind 
		 :edge-kind edge-kind))

(defun make-vertex (graph)
  (let ((vert (make-graph-vertex graph)))
    (insert-vertex graph vert)))

(defun connect-vertexes (graph vert1 vert2)
  "Returns an edge"
  (connect graph vert1 vert2))

(defun vertex-location(vert)
  (with-slots (location) vert
    location))

(defun vertex-edges (vertex)
  (with-slots (edges) vertex
    edges))

(defun get-edge(vertex1 vertex2)
  (with-slots(edges) vertex1
    (loop for edge in edges do
	 (with-slots(vertex-1 vertex-2) edge
	   (when (or (eq vertex-1 vertex2) (eq vertex-2 vertex2))
	     (return-from get-edge edge))))))

(defun graph-vertexes (graph)
  (with-slots (vertexes) graph
    vertexes))

(defun get-connected-verts(vertex)
  (loop for edge in (vertex-edges vertex) collect
       (with-slots (vertex-1 vertex-2) edge
	 (if (eq vertex-1 vertex)
	     vertex-2
	     vertex-1))))

(defun split-edge (edge graph)
  (with-slots(vertex-kind) graph
    (with-slots (vertex-1 vertex-2) edge
      (let ((vert (make-instance vertex-kind)))
      (setf (slot-value graph 'edges) 
	    (remove edge (slot-value graph 'edges)))
      (setf (slot-value vertex-1 'edges) 
	    (remove edge (slot-value vertex-1 'edges)))
      (setf (slot-value vertex-2 'edges) 
	    (remove edge (slot-value vertex-2 'edges)))
      (connect-vertexes graph vertex-1 vert)
      (connect-vertexes graph vert vertex-2)
      (on-split vert graph)
      vert))))

(defun load-test-graph ()
  (let ((grph (make-graph)))
    (let ((v1 (make-vertex grph))
	  (v2 (make-vertex grph))
	  (v3 (make-vertex grph))
	  (v4 (make-vertex grph))
	  (v5 (make-vertex grph))
	  (v6 (make-vertex grph))
	  )
    
      (connect-vertexes grph v1 v2)
      (connect-vertexes grph v2 v3)
      (connect-vertexes grph v3 v1)
      (connect-vertexes grph v3 v4)
      (connect-vertexes grph v4 v5)
      (connect-vertexes grph v5 v6)
      (connect-vertexes grph v6 v2)
      (test:assert (length (get-
      (test:assert (get-edge v1 v2) "Should be connected..")
      (split-edge (get-edge v1 v2) grph)
      (test:assert (not (get-edge v1 v2)) "Should'nt be connected")
      grph)))
      )))
(test:register 'load-test-graph)

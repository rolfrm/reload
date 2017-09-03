(defparameter test_tex (texture:make-file-texture "c:/Users/rolf/Dropbox/workspace/sbcl/reloadengine2/graph/concept/characters.png"))

(re.load "queue.lisp")
(re.load "graph-utils.lisp")
(re.load "graph-graphics.lisp")
(re.load "game-graph.lisp")
(re.load "graph-game-events.lisp")
(re.load "graph-occupier.lisp")
(re.load "graphical-model.lisp")

(defvar test-poly (make-instance 'polygon :vertexes square-vbo))

(deftask walk (obj start-vertex stop-vertex)
  :valid t 
  :exec (move-occupier obj start-vertex stop-vertex)
  :cost (vertex-distance start-vertex stop-vertex))

(deftask attack (attacker attacker-vertex target target-vertex weapon)
  :valid t ;actually check if target is attackable
  :exec ();(roll-attack obj target weapon)
  :cost (weapon-cost weapon)
  )
 
(defclass dropable-item (graph-occupier)
  ())

(defclass weapon (dropable-item)
  ((weapon-cost :initarg :cost :initform 0.0 :accessor weapon-cost)
   ))

(defmethod weapon-attack ((weapon weapon) origin-vertex target target-vertex)
  ())

(defclass fist (weapon) ())
(defmethod weapon-attack ((fist fist) origin-vertex target target-vertex)
  ())

(defclass player-object (graph-occupier)
   ((target-vertex :initform nil)))

(defmethod update-graph-occupier ((player player-object))
   (with-slots (target-vertex tasks (vertex current-vertex)) player
     (when vertex
       (if target-vertex
	   (progn
	     (setf tasks (make-queue))
	     (let ((path (a-star vertex target-vertex)))
	       (when (> (length path) 1)
		 (let ((last-vert nil))
		   (loop for vert in (reverse path) do
			(progn
			  (when last-vert
			    (enqueue-task player (walk player last-vert vert)))
			  (setf last-vert vert)
			  )))
		 ))
	     (setf target-vertex nil))
	   (unless (any-tasks player)
	     (enqueue-task 
	      player 
	      (walk player vertex
		    (utils:random-element (get-connected-verts vertex)))))))))

(defclass graph-renderer ()
  ())

(defmethod do-render-graph((graph-renderer graph-renderer) win)
  ())

(defvar *current-graph-game-renderer* (make-instance 'graph-renderer))
(defun set-current-graph-renderer(graph-renderer)
  (setf *current-graph-game-renderer* graph-renderer))

(re.load "graph-editor.lisp")
(re.load "graph-game-engine.lisp")



(defun reload-graph()
  (defparameter grph (load-test-graph))
  (defparameter player (list (make-player-object)))
  (defparameter enemies 
    (loop for asd from 0 below 1 collect 
	 (make-instance 'player-object 
			
			:offset (matrix:make-vector 
				 (list (utils:random-range -0.2 0.2) 
				       (utils:random-range -0.2 0.2)
				       (utils:random-range -0.2 0.2)))
			:model (make-instance 
				'graphical-model 
				:poly test-poly
				:color #(1.0 0.0 0.0 1.0)
				:scale (matrix:make-vector '(0.1 0.1 0.1))))))
  
  (with-slots (vertexes) grph
    (with-slots (occupiers) (first vertexes)
      (setf occupiers player))
    (with-slots (occupiers) (second vertexes)
      (setf occupiers enemies))
    )
  (load-graph-editor grph)
  )

(defvar font-tex nil)
(defun render (win)
  (sleep 0.01)
  (in-window win
	     (progn
	       (unless font-tex
		 (setf font-tex (ftgl:create-pixmap-font))
		 (ftgl:set-font-face-size font-tex 20 0))
	       ;(print "hej")
		;(print (load-from-context win tex1))
	       (texture:bind-texture tex1 win 0)
	       (show-window win)  
	       (gl:clear-color 0.2 0.2 0.2 1.0)
	       (gl:enable :blend)
	       (gl:blend-func :src-alpha :one-minus-src-alpha)
	       (gl:clear :color-buffer-bit)
	       (bind-program empty-prog win)
	       (gl:matrix-mode :projection)
	       (gl:load-identity)
	       (gl:matrix-mode :modelview)
	       
	       (gl:load-identity)
	       (gl:translate 0.0 0.0 1.0)
	       (gl:scale 1.0 1.0 1.0)
	       (gl:color 1.0 1.0 1.0 1.0)
	       (gl:raster-pos -1.0 -0.8)
	       (ftgl:render-font font-tex "0xFF123432" :all)
	       (gl:raster-pos -1.0 -0.9)
	       (ftgl:render-font font-tex "19929....." :all)
	       (do-render-graph *current-graph-game-renderer* win)
	       
	       (swap-buffers win)
	       (glfw:poll-events))))



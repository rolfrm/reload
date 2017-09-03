(defclass player (game-object) 
  ((target :initform (make-vector 0 0 0) )))

(def-behaviour player-regular (player)
  (with-slots (target location) player
    (loop while t do
	 (yeild)
	 (goto player target))))


(defmethod initialize-instance :after ((self player)&key)
  (with-slots (target location) self
    (add-behaviour self (player-regular self))
    (setf target location)
    ))


(defun make-player (loc)
  (make-instance 'player 
		 :max-speed 1.0
		 :location loc :color #(0.5 0.5 1.0 1.0)
		 :physical-body (make-aabb-object :size (make-vector 0.2 0.2 0.2)
						 :location loc)))


(defclass house (game-object)
  ())


(defclass npc (game-object)
  ((home :initarg :home)
   (inventory :initform nil)))

(defmethod initialize-instance :after ((self npc) &key)
  ())

(defun make-log(loc)
  (make-instance 'game-object 
		 :location loc
		 :size #(0.2 0.1)
		 :color #(0.6 0.4 0.1 1.0)))

(sub-behaviour chop-tree (lumberjack tree)
  (with-slots (world) lumberjack
    (wait-some 1)
    (let ((logs nil))
      (when (exists world tree)
	(remove-game-object world tree)
	(loop for x from 0 to (+ 1 (random 4)) do
	     (let ((new-log (make-log (lm:+ (lm:* 0.1 (random-location)) (slot-value tree 'location)))))
	       (push new-log logs)
	       (add-game-object world new-log))))
    
      logs)))

(sub-behaviour pick-up-object (npc object)
  (with-slots(inventory world) npc
    (when (exists world object)
      (remove-game-object world object)
      (wait-some 0.5)
      (push object inventory))))

(sub-behaviour drop-object (npc object)
  (with-slots(inventory world) npc
    (delete object inventory)
    (unless (exists world object)
      (setf (slot-value object 'location) (slot-value npc 'location))
      (add-game-object world object)
      (wait-some 0.5))))

(def-behaviour lumberjack-regular (lumberjack)
  (with-slots (world home) lumberjack
    
    (let ((lumberindex 0))
      (loop while t do
	   (let ((hour (hour-of-day (get-time lumberjack))))
	     (if (< -0.1 hour 8)
		 (let ((tree (find-nearest-of-kind world lumberjack 'tree)))
		   (if tree
		       (progn
			 (print "going to tree..")
			 (goto-object lumberjack tree)
			 (let ((pieces (chop-tree lumberjack tree)))
			   (print "tree chopped")
			   (loop for object in pieces do
				(goto-object lumberjack object)
				(print (format nil "picking up ~a" object))
				(pick-up-object lumberjack object))
			   (print "going home...")
			   (goto-object lumberjack house)
			   (goto lumberjack 
				 (lm:+ 
				  (make-vector 0.0 (* lumberindex 0.3) 0.0)
				  (slot-value lumberjack 'location)))
			   
			   (loop for object in pieces do
				(print (format nil "dropping object ~a" object))
				(goto lumberjack 
				      (lm:+ 
				       (make-vector 0.0 0.3 0.0)
				       (slot-value lumberjack 'location)))
				(incf lumberindex 1.0)
				(drop-object lumberjack object))))
		       (wait-some 1)))

		 (progn
		   (when home
		     (print "going home..")
		     (goto-object lumberjack home)
		     (print "arrived.."))
		   (wait-some 5))))))))
		       
(defun make-house (loc)
  (make-instance 'house :size #(2.0 3.0)
		 :location loc
		 :color #(0.5 0.5 0.5 1.0)))

(defun make-lumberjack (loc house)
  (let ((lumberjack
	 (make-instance 'npc :home house
		 :location loc
		 :physical-body (make-aabb-object :size (make-vector 0.5 0.5 0.1))
		 :max-speed 1.0)))
    (add-behaviour lumberjack (lumberjack-regular lumberjack))
    lumberjack))

(defclass wisp-ai (game-object)
  ((main-behaviour)))

(defun make-wisp (position)
  (make-instance 'wisp-ai
		 :location position
		 :physical-body (make-aabb-object :size (make-vector 0.2 0.2 0.2) :location position)
		 :size #(0.1 0.1)))

(defmethod initialize-instance :after ((self wisp-ai) &key (pos #(0 0 0)))
 ) ;(with-slots (main-behaviour) self
;    (setf main-behaviour (make-instance 'continuum :continuum (wisp-regular self)))
   ; (add-behaviour self main-behaviour)))

(defclass tree (game-object) 
  ((tree-size :initform 0.2)))

(defclass spider (game-object) ())

(defun make-tree (pos)
  (let ((out
	 (make-instance 'tree
			:size #(0.2 0.2)
			:location pos
			:color #(0.3 0.5 0.2 1.0))))
    (add-behaviour out (tree-growth out))
    out))

(def-behaviour tree-growth (tree)
  (loop while t do 
       (wait-some 1.0)
       (with-slots (tree-size size world location)
	   tree
	 (incf tree-size 
	       (if 
		(< tree-size 2.0)
		(random-range 0.0 0.1)
		(random-range 0.0 0.001)))
	 (setf size (vector tree-size tree-size))
	 (when (> tree-size 2.0)
	   (when (> 5.0 (random-range 0.0 10000.0))
	     (let ((new-tree (make-tree (lm:+ (lm:* (random-location) 0.5) location))))
	       (add-game-object world new-tree)))))))


(defmethod initialize-instance :after ((self spider) &key)
  (with-slots (active-behaviours) self
    (add-behaviour self (make-instance 'continuum :continuum (spider-behaviour self)))
    (add-behaviour self (spider-setcolor self))

    ))


(defun make-spider (pos)
  (make-instance 'spider
		 :size #(0.5 0.5)
		 :physical-body (make-aabb-object 
				 :size (make-vector 0.5 0.5 0.5)
				 :location pos)
		 :max-speed 1.5
		 
		 :location pos
		 :color #(0.2 0.2 0.2 1.0)))

(def-behaviour spider-setcolor (spider)
  (with-slots (location color) spider
    (loop while t do
	 (setf (aref color 0) (+ 0.5 (* 0.5 (sin (lm:x location)))))
	 (yeild))))

(def-behaviour spider-behaviour (spider)
  (with-slots (world) spider
    (loop while t do
	 (let ((edible (find-nearest-of-kind world spider 'wisp-ai)))
	   (if (and edible (< (object-distance edible spider) 80.0))
	       (progn
		 (goto-object spider edible)
		 (remove-game-object world edible)
		 )
	       (goto spider (make-vector 0.0 0.0 0.0)))  
	   )
	 (wait-some 0.5))))



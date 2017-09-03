(defclass lumber-jack (game:person)
  ())

;;behaviours. Some kind of very low priority thread.
(defbehaviour regular-behaviour ((guy2 lumber-jack) world)
  (progn 
    (loop while (< (get-time) (hour 16)) do
	 (go-to (find-nearest 'forest world guy2))
	 (let ((tree (find-nearest 'tree world guy2)))
	   (go-to tree)
	   (let ((tree-related (chop tree)))
	     (mapcar #'pick-up (remove-if-not (curry #is-type 'lumber) tree-related)))))
    (let ((house (find-home guy2 world)))
      (go-to house)
      (let ((front-door (find-on house world 'front-door)))
	(open-door front-door)
	(enter-door front-door)
	(close-door front-door))
      (let ((bed (find-on house world 'bed)))
	(loop while (< (get-time) (hour 6)) do
	     (sleep (hours 1)))))))
    
(defmethod on-spotted ((guy2 lumberjack) (w beast))
  (hide-from w))

(defmethod on-spotted ((guy2 lumberjack) (p person))
  (if (equal (get-fraction p) 'allied)
      (greet p)))
   

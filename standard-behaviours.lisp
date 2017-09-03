(sub-behaviour wait-some (seconds)
  (let ((end (+ seconds (get-exact-time))))
    (loop while (> end (get-exact-time)) do
	 (yeild))))

(defun distance(a b)
  (lm:norm (lm:- a b)))

(sub-behaviour go-to (unit target-location max-speed distance-threshold)
  (with-slots (location speed) unit
    (loop while (> (distance location target-location) distance-threshold) do
	 
	 (let ((dist (distance location target-location)))
	   (setf speed (lm:* (min dist max-speed) (lm:normalise (lm:- target-location location))))
	   (wait-some 0.01))
	 )
    (setf speed (make-vector 0.0 0.0 0.0))))

(sub-behaviour go-to-object (unit target max-speed distance-threshold)
  (loop while (>= (lm:norm (lm:- (slot-value unit 'location) (slot-value target 'location))) distance-threshold) do
       (go-to unit (lm:+ (lm:* 2.0 (slot-value target 'speed)) (slot-value target 'location)) max-speed 0.01)
       ))

(sub-behaviour goto (unit target-location)
  (with-slots (speed max-speed) unit
    (print unit)
    (go-to unit target-location max-speed (* 0.01 max-speed))))

(sub-behaviour goto-object (unit object)
  (with-slots (location speed max-speed) unit
    (print unit)
    (go-to-object unit object max-speed (* 0.01 max-speed))))

(defstruct aabb-object
  (size (lm:make-vector 3) :type lm:vector)
  (location (lm:make-vector 3) :type lm:vector)
  (speed (lm:make-vector 3) :type lm:vector)
  (mass 1d0 :type double-float)
  (is-dirty t :type boolean)) ;if speed has changed since last iteration

(defun set-velocity (aabb-object speed-vector)
  (unless (vec-eql (aabb-object-speed aabb-object) speed-vector)
    (setf (aabb-object-speed aabb-object) speed-vector)
    (setf (aabb-object-is-dirty aabb-object) t)))

(defun iterate-aabb (p-obj dt)
  (setf (aabb-object-location p-obj) 
	(lm:+ (aabb-object-location p-obj)
	      (lm:* (aabb-object-speed p-obj) dt))))

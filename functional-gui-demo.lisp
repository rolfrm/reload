(defpackage :fungui-demo
  (:use :cl :funtree :fungui :fungui-items))
(in-package :fungui-demo)

(defclass world ()
  ((sim-running :initform t)
   (scale :initform 1.0)
   (item-offset :initform (m:vec 0.0 0.0))))

(defun generate-scene-graph (world)
  (let ((vertex-code"
 attribute vec2 pos;
 uniform vec2 translate;
 uniform float rotate;
 uniform vec2 scale;
 void main(){
  float sinr = sin(rotate);
  float cosr = cos(rotate);
  mat3 mat = mat3(vec3(-sinr, cosr, 0.0f), 
		  vec3(cosr,sinr,0.0f), 
		  vec3(translate.xy, 1.0f));
 vec3 v = mat * vec3(pos * scale, 1.0f);
  gl_Position = vec4(v.xy / v.z,0.0f, 1.0f);
 }")
 (fragment-code"
 uniform vec3 color;
 void main(){
  gl_FragColor = vec4(color + vec3(0.5, 0.5, 0.5), 1.0);
 }")) 
    (node
     (standard-renderer)
     (node 
      (window :win1 :title "Window 1" :size (m:vec 200 200))
      (node
       (shader vertex-code fragment-code)
       
       (node 
	 (transform :scale (m:vec (slot-value world 'scale) (slot-value world 'scale)))
       (node
	
	 
       	(transform :scale (m:vec 1.0 -1.0))
	
	 (node
	  (transform :translation (slot-value world 'item-offset))
	  (buffer (m:matrix '((-0.5 -0.5) (-0.5 0.5) (0.5 0.5) (0.5 -0.5)))
		  :shader-identifier :attribute-loc-0)
       ))))))))


(defun update-world (world event-list)
  (sleep 0.05)
  (when event-list
    (loop for event in (utils:get-assoc :win1 event-list) do
	 (progn
	   ;(print event)
	   (when (eq (car event) :cursor-pos)
	     (let ((cursor-pos (cadr event)))
	       (setf (slot-value world 'item-offset) 
		     (m:- (m:scale cursor-pos 2/200) (m:vec 1.0 1.0)))))
	   (when (eq (car event) :scroll)
	     (with-slots(scale) world
	       (setf scale (* (+ 1 (* 0.1 (third event))) scale))))
	   (when (or ;(equal event '(:close))
		     (equal event '(:key :down :escape)))
	     (with-slots(sim-running) world
	       (setf sim-running nil))))))
  world)

(defun iterate-demo (world)
  (with-slots(sim-running) world
    (when sim-running
      (iterate-demo (update-world world (render-tree (generate-scene-graph world)))))))

(defun run-demo()
  (iterate-demo (make-instance 'world)))

;;A thing we can draw
(declaim (optimize (speed 3) (safety 0) (space 0) (debug 0)))

;(re.load "continuation2.lisp")
(defmacro def-behaviour (name args &body body)
  `(cl-cont:defun/cc ,name ,args
     (progn
       (yeild)
       ,@body)))

(defmacro sub-behaviour (name args &body body)
  `(cl-cont:defun/cc ,name ,args
     (progn
       ,@body)))

(defclass game-object (game-object-ai)
  ((location :initform (lm:make-vector 3) :initarg :location)
   (speed :initform (lm:make-vector 3) :initarg :speed)
   (max-speed :initform 0.0 :initarg :max-speed)
   (color :initform #(1.0 1.0 0.2 1.0) :initarg :color )
   (physical-body :initform nil :initarg :physical-body)
   (size :initform #(0.5 0.5) :initarg :size)))

(defmethod initialize-instance :after ((self game-object) &key)
  ()) 


(defun hour-of-day (time)
  (let ((hours (/ time 3600.0)))
    (mod hours 24)))

(defmethod get-time((gobj game-object))
  (with-slots (world) gobj
    (slot-value world 'time)))

(re.load "standard-behaviours.lisp")
(re.load "game1-player.lisp")


;;The world class
(defclass game-object-control ()
  ((-game-objects :initform nil :initarg :objects)
   (physical-map :initform (make-hash-table :test 'eql))
   (player-object :initarg :player)
   (time :initform 0.0)
   (physics-context :initform (make-instance 'physics-context))))

(defmethod exists ((game-control game-object-control) object)
  (with-slots(-game-objects) game-control
    (if (find object -game-objects)
	t
	nil)))

(defmethod game-step-ai ((game-control game-object-control))
  (with-slots (-game-objects) game-control
    (mapcar (rcurry #'step-behaviours game-control) -game-objects)))


(defmethod game-step ((game-control game-object-control) &optional (dt 0.5))
  (with-slots (-game-objects physics-context physical-map time) game-control
    ;first step ai
    (mapcar (rcurry #'step-behaviours game-control) -game-objects)
    ;(print -game-objects)
    ;step physics
    (let ((is-dirty nil))
      (loop for game-obj in -game-objects do
	   (with-slots (physical-body speed) game-obj
	     (let ((physical-entry (gethash game-obj physical-map)))
	       
	       (if physical-body
		   (progn
		     (set-velocity physical-body (slot-value game-obj 'speed))
		     (unless physical-entry
		       (setf is-dirty t)
		       (setf (gethash game-obj physical-map) physical-body)
		       (push physical-body (slot-value physics-context 'physics-objects)))
		     )
		   (when physical-entry
		     (setf is-dirty t)
		     (remove-physics-object context physical-body)
		     (remhash game-obj physical-map))))))
      (physics-iterate physics-context dt)
      (loop for gobj in (hash-table-keys physical-map) do
	   (setf (slot-value gobj 'speed) (aabb-object-speed (gethash gobj physical-map)))
	   (setf (slot-value gobj 'location) (aabb-object-location (gethash gobj physical-map))))
      (incf time (* 100 dt))
      )))
    

(defmethod get-game-objects ((game-control game-object-control))
  (with-slots (-game-objects) game-control
    -game-objects))

(defmethod remove-game-object ((game-control game-object-control) game-object)
  (with-slots (-game-objects) game-control
    (setf -game-objects (remove game-object -game-objects))))

(defmethod add-game-object ((game-control game-object-control) game-object)
  (with-slots (-game-objects) game-control
    (push game-object -game-objects)))


(defun get-exact-time()
  (multiple-value-bind (seconds microseconds)
      (get-time-of-day)
    (+  (coerce seconds 'double-float) (/ (coerce microseconds 'double-float) 1000000.0))))



(defun make-vector (&rest rest)
  (lm:make-vector (length rest) :initial-elements rest))

(defun random-location()
  (make-vector (random-range -20.0 20.0) (random-range -20.0 20.0) 0.0))

(defvar x-- 0)
(defun test-func ()
  (loop for y from 0 to 100000 do
	      (setf x-- (+ x-- y))))

(def-behaviour sleeper()
  (let ((x 10))
    (loop while t do
	 (loop for y from 0 to 10000 do
	      (setf x-- (+ x-- y)))
	 (yeild))))

(defun test-sleepers(iterations sleepers)
  (let ((sleepers (loop for x from 0 to sleepers collect (sleeper))))
    (print "testing..")
     (dotimes (n iterations)
       (setf sleepers (loop for slp in sleepers collect (funcall slp))))))

(defun test-non-sleeper (iterations sleepers)
  (loop for x from 0 to sleepers do
       (dotimes (n iterations)
	 (loop for y from 0 to 10000 do
	      (setf x-- (+ x-- y))))))


(def-behaviour wisp-regular (wisp)
  (with-slots ( main-behaviour world) wisp
    
    (with-slots (location speed) wisp
      (loop while t do
	   (go-to wisp (random-location) 0.03 0.1)
	   ))))


(defun object-distance (unit1 unit2)
  (lm:norm (lm:- (slot-value unit1 'location) (slot-value unit2 'location))))


(defun find-nearest-of-kind(world unit kind)
  (first (sort (remove-if-not (lambda (x) (equal (type-of x) kind)) (get-game-objects world))
	       #'<
	:key (lambda (x) (lm:norm (lm:- (slot-value x 'location) (slot-value unit 'location)))))))
		 
(defparameter to-remove nil)



(defvar frag-obj (make-instance 'gpu-program-object :kind :fragment))
(defvar vert-obj (make-instance 'gpu-program-object :kind :vertex))

(set-shader-code frag-obj "
uniform vec4 color;
uniform sampler2D sampler;
uniform sampler2D sampler2;
varying vec2 uv;
void main(){
  
  gl_FragColor = color;// + texture2D(sampler,uv);// + texture2D(sampler2,uv);
}")
(set-shader-code vert-obj "
attribute vec2 pos;
uniform vec2 offset;
uniform vec2 cameraPos;
uniform float cameraZoom;
varying vec2 uv;
uniform vec2 size;
void main(){
  uv = pos / 1.0;
  gl_Position = vec4((pos*size + offset - cameraPos)*cameraZoom ,0.0,1.0);

}
")

(defvar gpu-prog (make-instance 'gpu-program :vertex vert-obj :fragment frag-obj))
(defvar vbo (make-instance 'gpu-array))
(set-vertexes vbo (list -1.0 -1.0 1.0 -1.0 1.0 1.0 -1.0 1.0))
(defun set-blend-mode(win)
  (in-window win
	     (progn
	       ;(gl:clear-color 0.0 0.0 0.0 1.0)
	       (gl:enable :blend)
	       (gl:blend-func :src-alpha :dst-alpha) ;;Additive alpha blending
	       ;(gl:blend-func :one :one) ;;Additive blending
	       ;(gl:blend-func :one :one) ;;Additive (1 - alpha) blending
	       ;(gl:blend-func :src-alpha :one-minus-src-alpha) ;transparancy
	       ;(gl:blend-func :one-minus-src-alpha :src-alpha)
	       
	       )))
(set-dimension vbo 2)
(defvar it 0.0)
(setf it 0.0)
(defun load-game ()
 ; (load-simple-game)
  ;(return-from load-game)

  (progn
    (print "...")
    (defparameter player (make-player (random-location)))
    (defparameter house (make-house (random-location)))
    (defparameter lumberjack (make-lumberjack (random-location) house))

    (defparameter game-objects (list 
				house
				lumberjack
				player
				(make-wisp (random-location)) (make-wisp (random-location))  (make-wisp (random-location))  
				(make-spider (random-location))(make-spider (random-location))
				(make-tree (random-location)) (make-tree (random-location))
				(make-tree (random-location)) (make-tree (random-location))
				(make-tree (random-location)) (make-tree (random-location))
				(make-tree (random-location)) (make-tree (random-location))
				(make-tree (random-location)) (make-tree (random-location))
				(make-tree (random-location)) (make-tree (random-location))
				
				(make-tree (random-location)) (make-tree (random-location))))

    (print ".....")
    (defparameter game-manager (make-instance 'game-object-control :objects game-objects :player player))))

(defparameter camera-zoom 0.025)
(defvar camera-pos (make-vector 0 0))
(defun render (win)
  (set-blend-mode win)
   (glfwshowwindow (slot-value win 'glfw-window-ref))
   (sleep (/ 2 60))
  ;(glfwSwapInterval 0)
   (set-uniform gpu-prog "cameraZoom" camera-zoom)
   (with-slots(player-object) game-manager
     (with-slots (location) player-object
       (setf camera-pos (make-vector (lm:x location) (lm:y location)))
       (set-uniform gpu-prog "cameraPos" (lm:x location) (lm:y location))))
   (game-step game-manager 0.1)
  (loop for x from 0 to 0 do
       (in-window win (gl:clear :color-buffer-bit))
       ;(gl:blend-func :one :zero)
       (loop for game-obj in (slot-value game-manager '-game-objects) do
	    ;(print game-obj)
	    (with-slots(location color size) game-obj
	      (let ((loc location))
		(set-uniform gpu-prog "size" (aref size 0) (aref size 1))
		(set-uniform gpu-prog "color" (aref color 0) (aref color 1) (aref color 2) (aref color 3))
		(set-uniform gpu-prog "offset" (lm:elt loc 0) (lm:elt loc 1))
		(bind-program gpu-prog win)
		(bind-array vbo win 0)
		(in-window win
			   (gl:draw-arrays :triangle-fan 0 4)))))
       (swap-buffers win   ))

  (mapcar (curry #' remove-game-object game-manager) to-remove)
  (setf to-remove nil)
  
  (glfwpollevents)
  )

(defvar mouse-pos (make-vector 0 0))

(defun screen-to-world (vec)
  (let ((size (apply #'make-vector (get-window-size win))))
    (let ((ncc (lm:- (lm:/ vec size) (make-vector 0.5 0.5))))
      
      (setf (lm:y ncc) (- 0.0 (lm:y ncc)))
      (lm:+  (lm:* (/ 1.0 camera-zoom) 2.0 ncc) camera-pos))))

(defun mouse-click (button action)
  (with-slots (player-object) game-manager
    (with-slots (target) player-object
      (let ((2dtarget (screen-to-world mouse-pos)))
	(print "click!")
	(setf target (make-vector (lm:x 2dtarget) (lm:y 2dtarget) 0))))))
(defun mouse-move (x y)
  (setf mouse-pos (make-vector x y)))

(defun key-action (key action)
  (when (equal action 1)
    ;(load-game)
    ))

(defun test-physics (times)
  (loop for x from 0 to times do
       (print "LOADING SIMPLE GAME")
       (load-simple-game)
       (let ((ok nil))
	 (handler-case
	     (progn
	       (game-step game-manager 0.01)
	       (setf ok t))
	   (error (e) (print e)))
	 (when ok
	   (game-step game-manager 5.0)))))

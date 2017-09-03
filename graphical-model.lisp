(defclass polygon ()
  ((vertexes :initform nil :initarg :vertexes)
   (uvs :initform nil :initarg uvs)
   (gl-draw-kind :initform :points :initarg :draw-kind)))

(defclass graphical-model ()
    ((polygon :initform nil :initarg :poly)
     (color :initform #(1.0 1.0 1.0 1.0) :initarg :color)
     (offset :initform (matrix:make-vector '(0.0 0.0 0.0)) :initarg :offset)
     (scale :initform (matrix:make-vector '(1.0 1.0 1.0)) :initarg :scale)
     (sub-models :initform nil)))

(defclass graphical-context ()
  ((shader :initarg :shader :initform nil)
   (window :initarg :window)))

(defmethod uniform ((context graphical-context) name &rest values)
  (with-slots (shader window) context
    (in-window window
	       (apply #'set-uniform shader name values))))

(defmethod bind-texture((context graphical-context) texture channel)
  (with-slots (shader window)
      (in-window window
		 (texture:bind-texture texture window channel))))

(defmethod draw-polygon ((context graphical-context) (polygon polygon))
  (with-slots (shader window) context
    (with-slots (vertexes uvs gl-draw-kind) polygon
      (when vertexes
	(in-window window
	   (bind-array vertexes win 0)
	   (bind-program shader win)
	   (gl:draw-arrays gl-draw-kind 0 
			   (gpu-array-length vertexes)))))))

(defun vector-to-list (vector)
  (enumerate:to-list (enumerate:take 2 (enumerate:walk-array  (matrix:matrix-data vector)))))

(defmethod render-graphical-model((model graphical-model) graphical-context rel-offset rel-scale)
  (with-slots(polygon offset scale sub-models color) model
    (let* ((new-scale (matrix:* rel-scale scale))
	   (new-offset (matrix:+ rel-offset (matrix:* new-scale offset) 
				))) 
      (uniform graphical-context "color" 
	       (aref color 0) (aref color 1) (aref color 2) 0.5)
      (apply #'uniform graphical-context "offset" (vector-to-list new-offset))
      (apply #'uniform graphical-context "size" (vector-to-list new-scale))
      (draw-polygon graphical-context polygon)
      (loop for sub-model in sub-models do
	   (render-graphical-model graphical-context sub-model new-offset new-scale))
      )))

(defstruct animation-frame 
  (length 1.0)
  (start) ;2d uv vector
  (stop)) ;2d uv vector

(defclass Sprite (polygon)
  ((last-time :initform 0.0)
   (frames :initform nil)))

(defclass still-sprite (polygon)
  ((frame :initarg :frame)
   (texture :initform :tex)))

(defmethod render-graphical-model :before 
    ((model still-sprite) graphical-context rel-offset rel-scale)
  (with-slots (frame texture) graphical-context
    (bind-texture graphical-context texture)
    (flet ((unif2 name x y)
	   (uniform graphical-context name x y))
      (unif2  "uv_start" 0.0 0.0)
      (uniform graphical-context "uv_stop" 0.0 0.0)
      (uniform graphical-context "tex_size" 0 0)
    )))

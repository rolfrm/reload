(defpackage :functional-gui-items
  (:nicknames :fungui-items)
   (:use :cl)
   (:export :leaf-renderable :transform :shader
	    :buffer :uniform :standard-renderer)
    )
 (in-package :functional-gui-items)

 ;;*** Leaf renderable ***;;;
 (defclass leaf-renderable ()
   ())

 (defmethod funtree:load-element ((leaf leaf-renderable) context child-elements)
   (let ((returned (call-next-method)))
     (unless child-elements
       (let ((renderer (utils:get-assoc :renderer context)))

	 (when renderer
	   (funcall renderer context))))
     returned))

 (defun render-context (context)
   )

 ;;** Transform **;
 (defclass transform (leaf-renderable)
     ((rotation :initform 0.0 :initarg :rotation)
      (translation :initform (m:vec 0.0 0.0) :initarg :translation)
      (scale :initform (m:vec 1.0 1.0) :initarg :scale)))

 (defun transform (&key 
		     (rotation 0.0)
		     (scale (m:vec 1.0 1.0))
		     (translation (m:vec 0.0 0.0)))
   (make-instance 'transform 
		  :rotation rotation
		  :scale scale
		  :translation translation))

 (defmethod funtree:modify-context ((tform transform) context) context)

 (defmethod funtree:load-element ((tform transform) context child-elements)
   (with-slots ((tt translation)
		(tr rotation)
		(ts scale)) tform
	    (utils:let-list 
	     (translate rotate scale) 
	     (utils:get-assocs context 'transform 'rotation 'scale)
	     (let((new-translate
		   (if translate
			 (m:+ translate (m:* scale 
					     (mex:rotate-vector tt rotate)))
			 tt))
		  (new-rotate 
		   (if rotate
		       (+ rotate tr)
		       tr))
		  (new-scale (if scale 
			 (m:* scale ts)
			 ts)))

	     (let ((new-context
		    (utils:add-assocs 
		     context
		     'transform 
		     new-translate
		     'rotation 
		     new-rotate
		     'scale 
		     new-scale)))
	       (utils:assoc-bind 
		((shader :shader)) context
		(with-slots(gl-ref) shader
		  (set-uniform-by-name gl-ref "translate" new-translate)
		  (set-uniform-by-name gl-ref "scale" new-scale)
		  (set-uniform-by-name gl-ref "rotate" new-rotate)
		  (call-next-method tform new-context child-elements)
		  (when translate
		    (set-uniform-by-name gl-ref "translate" translate)
		    (set-uniform-by-name gl-ref "scale" scale)
		    (set-uniform-by-name gl-ref "rotate" rotate)))
	      ))))))


 (defclass render-output-test (leaf-renderable)
   ((last-context)))

 (defmethod funtree:load-element :before ((rot render-output-test) context child-elements)
   (with-slots (last-context) rot
     (setf last-context context)))

 ;;***********Shader***********;;
 (defclass shader(leaf-renderable)
   ((vertex-code :initarg :vertex-code)
    (fragment-code :initarg :fragment-code)))

 (defmethod funtree:modify-context ((shader shader) context)
   context)

 (defun load-and-bind-shader(shader win-ctx)
   (unless (gethash :shader-manager win-ctx)

     (setf (gethash :shader-manager win-ctx)

	   (item-manager:make-item-manager #'load-shader 
					   #'delete-shader
					   5
					   #'hash-shader)))
   (let ((shader-manager (gethash :shader-manager win-ctx)))
     (let ((shader (item-manager:get-item shader-manager shader)))
       shader)))

 (defmethod funtree:load-element :around ((shader shader) context child-elements)
   (utils:assoc-bind 
    ((window :window) (old-shader :shader)) context
    (if (null window)
	(error "No context for shader")
	(with-slots ((win-ctx fungui:context)) window
	  (let ((loaded-shader (load-and-bind-shader shader win-ctx)))
	    (bind-shader loaded-shader)
	    (call-next-method shader (utils:add-assoc :shader loaded-shader context) child-elements)
	  (if old-shader
	      (bind-shader old-shader win-ctx)
	      (gl:use-program 0)))))))

 (defun shader (vertex-code fragment-code)
   (make-instance 'shader 
		  :vertex-code vertex-code
		  :fragment-code fragment-code))

 (defclass shader-prog ()
   ((gl-ref :initform 0 :initarg :gl-ref)
    (vertex-log :initarg :vertex-log)
    (fragment-log :initarg :fragment-log)
    (shader-ok :initarg :shader-ok))
 )

 (defun compile-shader-code (kind shader-str)
   (let ((shader-part (gl:create-shader kind)))
     (gl:shader-source shader-part shader-str)
     (gl:compile-shader shader-part)
     shader-part))

 (defun shader-from-components(frag-shader vert-shader)
   (let ((program (gl:create-program)))
     (gl:attach-shader program vert-shader)
     (gl:attach-shader program frag-shader)
     (gl:link-program program)
     program))

 (defun print-shader-log(vertex-log fragment-log)
   (print "Vertex shader log:")
   (print vertex-log)
   (print "Fragment shader log:")
   (print fragment-log))

 (defun shader-from-component-code (fragment-code vertex-code)
   (let ((frag-shader (compile-shader-code :fragment-shader fragment-code ))
	 (vert-shader (compile-shader-code :vertex-shader  vertex-code)))
     (let ((vc-log (gl:get-shader-info-log vert-shader))
	   (fc-log (gl:get-shader-info-log frag-shader)))
	   (let ((shader (shader-from-components vert-shader frag-shader)))
	     (let ((link-status (gl:get-program shader :link-status)))
	       (unless link-status
		 (print "Error compiling shader:")
		 (print-shader-log vc-log fc-log))
	       (gl:delete-shader vert-shader)
	       (gl:delete-shader frag-shader)
	       (make-instance 'shader-prog 
			      :gl-ref shader
			      :vertex-log vc-log
			      :fragment-log fc-log
			      :shader-ok link-status)
	     )))))

 (defun load-shader(shader-proto)
   (with-slots ((v vertex-code) 
		(f fragment-code)) shader-proto
     (shader-from-component-code f v)
     ))

 (defun delete-shader (shader)
   (with-slots (gl-ref) shader
     (unless (eq gl-ref 0)
       (gl:delete-program gl-ref))))

 (defun hash-shader (shader-proto)
   (with-slots ((v vertex-code) 
		(f fragment-code)) shader-proto
     (concatenate 'string v f)))

 (defun bind-shader (shader)
   (with-slots(gl-ref) shader
     (gl:use-program gl-ref)))

 ;;** Buffer **;;
 (defclass buffer (leaf-renderable)
   ((values :initform (m:matrix) :initarg :values)
    (program-identifier :initarg :program-identifier :initform :attribute-loc-0)))

 (defun buffer (matrix &key (shader-identifier 0))
   (make-instance 'buffer
		  :values matrix
		  :program-identifier shader-identifier))

 (defun get-program-identifier-loc(shader-ref program-loc)
   (cond
     ((stringp program-loc) (gl:get-attrib-location  shader-ref program-loc))
     ((keywordp program-loc)
		(case program-loc 
		  (:attribute-loc-0 0) 
		  (:attribute-loc-1 1)
		  (otherwise (error "unknown program loc ~a" program-loc))))
     ((integerp program-loc) program-loc)
     ((t (error "Unsupported location type ~a" program-loc)))))


 (defun bind-vbo (vbo buffer shader)
   (with-slots(program-identifier) buffer
     (with-slots(ref dim) vbo
       (with-slots (gl-ref) shader
	 (let ((progloc (get-program-identifier-loc gl-ref program-identifier )))
	 (gl:bind-buffer :array-buffer ref)
	 (gl:enable-vertex-attrib-array 0)
	 (gl:vertex-attrib-pointer progloc dim :float nil 0 (cffi:null-pointer))
	 )))))

 (defmethod funtree:modify-context ((buffer buffer) context)
   context)

 (defun load-and-bind-vbo (buffer win-ctx)
   (unless (gethash :vbo-manager win-ctx)
     (setf (gethash :vbo-manager win-ctx)
	   (item-manager:make-item-manager #'load-vbo
					   #'delete-vbo
					   5
					   #'hash-vbo)))

   (let ((vbo-manager (gethash :vbo-manager win-ctx)))
     (let ((vbo (item-manager:get-item vbo-manager buffer)))
       vbo)))


 (defmethod funtree:load-element ((buffer buffer) context child-elements)
   (with-slots(program-identifier) buffer
     (utils:assoc-bind 
      ((window :window) 
       (old-vbo program-identifier)
       (old-shader :shader))context
       (if (null window)
	   (error "No context for VBO")

	     (let ((loaded-vbo 
		    (with-slots ((win-ctx fungui:context)) window
		      (load-and-bind-vbo buffer win-ctx))))
	       (bind-vbo loaded-vbo buffer old-shader)
	       (call-next-method buffer (utils:add-assoc program-identifier loaded-vbo context) child-elements)
	       (when old-vbo
		 (bind-vbo old-vbo buffer old-shader))
	       )))))


 (defstruct vbo
   (dim 0)
   (length 0)
   (ref 0)
   )

 (defun load-vbo (buffer)
   (with-slots ((matrix values)) buffer
   (let ((shape (m:get-2d-shape matrix))
	 (ref (first (gl:gen-buffers 1)))
	 (data (m:matrix-data matrix)))
     (gl:bind-buffer :array-buffer ref)
     (gl:with-gl-array (arr :float :count (* (aref shape 1)
					     (aref shape 0)))
       (dotimes (i (* (aref shape 1) (aref shape 0)))
	 (setf (gl:glaref arr i) (aref data i)))
       (gl:buffer-data :array-buffer :dynamic-draw arr))
     (gl:bind-buffer :array-buffer 0)
     (make-vbo :dim (aref shape 1) :length (aref shape 0) :ref ref))))

 (defun delete-vbo (vbo)
   (with-slots(ref) vbo
     (gl:delete-buffers (list ref))))

 (defun array2list (array)
   (map 'list (lambda (x) x) array))

 (defun hash-vbo (buffer)
   (with-slots (values program-identifier) buffer
     (append (array2list (m:matrix-data values))
	     (array2list (m:matrix-shape values))
	     (list program-identifier))))

 (defun draw-vbos (draw-kind &rest vbos))

 ;; ** Uniform ** ;;
 (defclass uniform (leaf-renderable)
   ((name :initform "" :initarg :name)
    (value :initform nil :initarg :value)))

 (defun uniform (name value)
   (make-instance 'uniform :name name :value value))

 (defun set-uniform-matrix(loc matrix)
   (let ((shape (matrix:get-2d-shape matrix)))
     (if (eq (aref shape 1) 1)

	 (let ((datalst (map 'list (lambda (x) x) (m:matrix-data matrix))))
	   (if (> (length datalst) 4)
	       (error "Unsupported vector size")
	       (apply #'gl:uniformf loc datalst)))

	 (progn
	   (unless (eq (aref shape 0) (aref shape 1)) 
	     (error "Matrix must be square"))
	   (let ((dim (aref shape 0)))
	     (unless (or (eq dim 2) (eq dim 3) (eq dim 4))
	       (error "Unsupported matrix size ~a" dim))
	     (gl:uniform-matrix loc dim (matrix:matrix-data matrix)))))))

 (defun set-uniform (program-ref loc value)
   (cond 
     ((integerp value) (gl:uniformi loc value))
     ((floatp value) (gl:uniformf loc value))
     ((m:matrix-p value) (set-uniform-matrix loc value))
     ((listp value) (apply #'gl:uniformf loc value))
     (t (error "Unsupported type"))))

 (defun get-uniform-location (uniform-name program-ref)
   (gl:get-uniform-location program-ref uniform-name))

 (defun set-uniform-by-name(program-ref name value)
   (set-uniform program-ref (get-uniform-location name program-ref) value))

 (defun use-uniform (uniform program)
   (with-slots (name value) uniform
     (with-slots (gl-ref) program
       (set-uniform-by-name gl-ref name value))))

 ;(defmethod funtree:modify-context ((uni uniform) context)
 ;  (set-context-uniform uni context))
 (defmethod funtree:modify-context ((uniform uniform) context) context)
 (defmethod funtree:load-element :around ((uniform uniform) context child-elements)
   (with-slots (name) uniform
     (let ((old-uniform (utils:get-assoc (list :uniform name) context)))
       (utils:assoc-bind 
	((shader :shader)) context
	(use-uniform uniform shader)
	(let ((out (call-next-method uniform (utils:add-assoc (list :uniform name) uniform context) child-elements)
		(when old-uniform 
		  (use-uniform old-uniform shader))))
	  out)))))

;; ** Standard Renderer ** ;;
 (defclass standard-renderer(leaf-renderable)
   ())

(defun standard-renderer () (make-instance 'standard-renderer))

(defun standard-render-fcn (context)
  (utils:assoc-bind 
   (
    (current-vbo :attribute-loc-0)
    )
   context
   (with-slots (length) current-vbo
     (gl:draw-arrays :quads 0 length))))


(defmethod funtree:modify-context ((rnd standard-renderer) context)
  (with-slots(render-fcn) rnd
    (utils:add-assoc :renderer #'standard-render-fcn context)))



 ;; ** TESTS ** ::
 (defun functional-gui-test()
   "(0.0 0.0) --T(0.5 0.5)--> 0.5,0.5 ->R(pi) -> (0.5 0.5) --T(1.0 1.0)--> (-0.5 -0.5)
 --S(0.5 0.5) ->
 -> T(1.0 1.0) -> (-1.0 -1.0)
 -> T(2.0 2.0) -> (-1.5 -1.5)"

   (let ((test-node-1 (make-instance 'render-output-test))
	 (test-node-2 (make-instance 'render-output-test)))
     (funtree:render-tree 
      (funtree:scene-node 
       (transform :rotation pi :translation (m:vec 0.5 0.5)) 
       (funtree:scene-node
	(transform :translation (m:vec 1.0 1.0) )
	(funtree:scene-node
	 (transform :scale (m:vec 0.5 0.5))
	 (funtree:scene-node
	  (transform :translation (m:vec 1.0 1.0))
	  test-node-1)
	 (funtree:scene-node
	  (transform :translation (m:vec 2.0 2.0))
	  test-node-2)
	 ))))
     (mex:with-vector-reader t1     
       (with-slots (last-context) test-node-1
	 (utils:get-assoc 'transform last-context))

       (mex:with-vector-reader t2     
	 (with-slots (last-context) test-node-2
	   (utils:get-assoc 'transform last-context)) 

	 (assert (utils:float-equal (t1 0) (t1 1)))
	 (assert (utils:float-equal (t2 0) (t2 1)))
	 (assert (utils:float-equal (t1 0) -1.0))
	 (assert (utils:float-equal (t2 0) -1.5))
     ))))


 (test:register 'functional-gui-test)

 (defun functional-shader-test()
   (let ((orig-vertex "asd") (orig-fragment "bsd"))
     (let ((test-node-1 (make-instance 'render-output-test)))
       (funtree:render-tree 
	(funtree:scene-node 
	 (shader orig-vertex orig-fragment) 
	 test-node-1))
       (with-slots(last-context) test-node-1
	 (let ((shader (utils:get-assoc :shader last-context)))
	   (with-slots (vertex-code fragment-code) shader
	     (assert (string-equal vertex-code orig-vertex))
	     (assert (string-equal fragment-code orig-fragment))
	     ))))))

 (test:register 'functional-shader-test)
 (defparameter test-vertex-code
  "
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
 vec3 v = mat * vec3(pos, 1.0f);
  gl_Position = vec4(v.xy / v.z * 0.6,0.0f, 1.0f);
 }")
 (defparameter test-fragment-code
 "
 uniform vec3 color;
 void main(){
  gl_FragColor = vec4(color + vec3(0.5, 0.5, 0.5), 1.0);
 }")

 (defun test-1-render (context)
   (utils:assoc-bind 
    ((shader :shader)) context
    (let ((loaded-shader (load-shader shader)))
      (let ((ok (with-slots (shader-ok) loaded-shader shader-ok)))       
	(bind-shader loaded-shader)
	(delete-shader loaded-shader)
	(test:assert ok "Error in shader")
	(hash-shader shader)
	))))

 (defun test-2-render (context)
   (utils:assoc-bind 
    ((shader :shader) (window :window)) context
    (test:assert (and shader window)) ;;;Error is window/utils:assoc-bind
    (test:assert (and (not (keywordp shader)) (not (keywordp window))))
    (with-slots ((context2 fungui:context)) window
      (unless (gethash :shader-manager context2)

	(setf (gethash :shader-manager context2)

	      (item-manager:make-item-manager #'load-shader 
					      #'delete-shader
					      5
					      #'hash-shader)))
      (let ((shader-manager (gethash :shader-manager context2)))
	(let ((output-shader (item-manager:get-item shader-manager shader)))
	  (bind-shader output-shader)
	  (assert (typep output-shader 'shader-prog)) ;;enusre type
	)))))

 (defclass test-renderer(leaf-renderable)
   ((render-fcn :initarg :render-fcn)))

 (defmethod funtree:modify-context ((rnd test-renderer) context)
   (with-slots(render-fcn) rnd
     (utils:add-assoc :renderer render-fcn context)))

 (defun functional-shader-load-test()
   (let ((shader-node (shader test-vertex-code test-fragment-code))
	 (window (fungui:make-window :size (m:vec 100 100)
				     :title "Shader test")))
     (funtree:render-tree 
      (funtree:scene-node 
       (make-instance 'test-renderer :render-fcn #'test-1-render)
       (funtree:scene-node window shader-node)) 
      nil nil)))

 (test:register 'functional-shader-load-test)


 (defun functional-shader-load-test2()
   (let ((shader-node (shader test-vertex-code test-fragment-code))
	 (window (fungui:make-window :size (m:vec 100 100)
				     :title "Shader test")))
     (funtree:render-tree 
      (funtree:scene-node 
       (make-instance 'test-renderer :render-fcn #'test-2-render)
       (funtree:scene-node window shader-node)) 
      nil nil)))

 (test:register 'functional-shader-load-test2)

 (defun vbo-render (context)
   (utils:assoc-bind 
    (
     (vertexes :attribute-loc-0)
     (shader :shader)
     (uvs :attribute-loc-1)
     )
    context
    (let ((vbo (load-vbo vertexes)))
      (delete-vbo vbo)
      (hash-vbo vertexes)
      )))

 (defun vbo-render-managed (context)
   (utils:assoc-bind 
    (
     (vertexes :attribute-loc-0)
     (shader :shader)

     (uvs :attribute-loc-1)
     (window :window)
     )
    context
    (with-slots ((win-ctx fungui:context)) window
      (unless (gethash :vbo-manager win-ctx)
	(setf (gethash :vbo-manager win-ctx)
	      (item-manager:make-item-manager #'load-vbo
					      #'delete-vbo
					      5
					      #'hash-vbo)))
      (let ((vbo-manager (gethash :vbo-manager win-ctx)))
	(let ((output-vbo (item-manager:get-item vbo-manager vertexes)))
	  (assert (typep output-vbo 'vbo)))))))

 (defvar testwin (fungui:make-window :size (m:vec 100 100) :title "VBO test"))
 (defun functional-buffer-load-test(render-fcn)
   (let ((buffer-node 
	  (buffer (m:matrix 
		   '((0.0 0.0) (1.0 0.0) (1.0 1.0) (0.0 1.0))
		   ) :shader-identifier :attribute-loc-0))
	 (buffer-node-2 
	  (buffer (m:matrix 
		   '((-1.0 -1.0) (0.0 -1.0) (0.0 0.0) (-1.0 0.0))
		   ) :shader-identifier :attribute-loc-0))
	 (uniform-node (uniform "color" '(0.0 0.5 0.0)))
	 (uniform-node-1 (uniform "color" '(0.5 0.0 0.0)))
	 (window testwin)
	 (shader-node (shader test-vertex-code test-fragment-code)))
     (let ((tree (funtree:scene-node 
       (make-instance 'test-renderer :render-fcn render-fcn)
       (funtree:scene-node
	window
	(funtree:scene-node
	 shader-node
	 (funtree:scene-node
	  uniform-node-1
	 (funtree:scene-node
	  uniform-node
	  buffer-node)
	 buffer-node-2))))))
       (loop for x from 0 to 1 by 0.1 do
	    (progn
	      (with-slots(value) uniform-node
		(setf value (list x x 0.0)))
	      (with-slots(value) uniform-node-1
		(setf value (list 0.0 x x)))
	      (funtree:render-tree tree)
	      (sleep 0.5)))
      )))

 (defun actual-render (context)
   (utils:assoc-bind 
    (
     (current-vbo :attribute-loc-0)
     )
    context
    (with-slots (length) current-vbo
      (gl:draw-arrays :quads 0 length))))

 ;(test:register 'functional-buffer-load-test #'vbo-render #'vbo-render-managed #'actual-render )
 (test:register 'functional-buffer-load-test #'actual-render )

 (defun test-fcn ()
   (with-package:with-package :funtree
     (loop for x from 0 to 65.0 by 0.1 append
	  (print
	   (render-tree 
	 (scene-node
	  (make-instance 'test-renderer :render-fcn #'actual-render)
	  (scene-node 
	   (fungui:make-window :size (m:vec 400 400) :title "window" :win-id 'win3)
	   (scene-node 
	    (shader test-vertex-code test-fragment-code)
	    (scene-node
	     (transform :rotation (* 0.3 x) :translation (m:vec 0.0 0.1))
	     (scene-node
	      (transform :translation (m:vec 0.0 0.3) :rotation (* 0.2 x))
	      (scene-node
	       (transform :rotation (* -0.4 x) :translation (matrix:vec 0.2 -0.0))
	       (scene-node
		(transform :translation (m:vec -0.5 -0.5))
		(buffer (m:matrix '((0.0 -0.0)
				    (1.0 0.0) 
				    (1.0 1.0) 
				    (0.0 1.0)
				    )) 
			:shader-identifier :attribute-loc-0)
		))))))))))))



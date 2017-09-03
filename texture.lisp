(defpackage :texture 
  (:use :cl :context-manager :window)
  (:export :texture :raw-texture :set-data :get-data 
	   :file-texture :bind-texture :make-file-texture
	   :set-internal-format
	   :set-min-filter
	   :set-mag-filter
))
(in-package :texture)
(defclass texture (g-object)
  ((width :initform 0 :initarg :width)
   (height :initform 0 :initarg :height)
   (channels :initform 4 :initarg :channels)
   (min-filter :initform :linear :initarg :min-filter)
   (mag-filter :initform :linear :initarg :mag-filter)
   (internal-format :initform :rgb :initarg format)
   ))

(defclass raw-texture (texture)
  ((data :initform nil :initarg :data 
	 :reader get-data 
	 :writer set-data)))

(utils:export-class 'raw-texture)
(utils:export-class 'texture)

(defmethod bind-texture ((self texture) window loc)
  (let ((gl-tex (load-from-context window self)))
    (with-slots (reference) gl-tex
      (gl:active-texture (+ loc (glfw:gl-enum-value :texture0)))
      (gl:bind-texture :texture-2d reference))))


(def-g-object-setter raw-texture width new-width 
  (unless (> new-width 0) 
    (error (format nil "unsupported width ~a" width))))

(def-g-object-setter raw-texture height new-height
  (unless (> new-height 0) 
    (error (format nil "unsupported height ~a" height))))

(def-g-object-setter texture channels)
(def-g-object-setter texture min-filter)
(def-g-object-setter texture mag-filter)
(def-g-object-setter texture internal-format)
(def-g-object-setter texture data)

(defmethod set-data ((self raw-texture) new-data)
  (with-slots (data) self
    (increment-rev self)
    (setf data (copy-seq new-data))))

(defvar *watched-files* (make-hash-table :test 'equal))
(defvar *texture-monitor* nil)
(defvar *texture-load-dispatcher* nil)

(defun on-texture-changed (path)
  (let ((tex (gethash path *watched-files*)))
    (increment-rev tex)))


(unless *texture-monitor*
  (multiple-value-bind (dispatch reloader)
      (reload-engine:file-monitor-thread (lambda (path) t)
					 (lambda (path) (on-texture-changed path))
					 0.5 "texture reload thread")
  (setf *texture-load-dispatcher* dispatch)
  (setf *texture-monitor* reloader)))

(defclass file-texture (texture)
  ((path :initarg :path :initform (error "not path supplied"))))

 (defun make-file-texture (path)
   (let ((from-hash (gethash path *watched-files*)))
     (if from-hash
	 from-hash
	 (let ((ftex (make-instance 'file-texture :path path)))
	   (setf (gethash path *watched-files*) ftex)
	   (file-monitor:add-watched-file *texture-monitor* path)
	   ftex))))

(defclass gl-texture ()
  ((width :initform 0 :initarg :width)
   (height :initform 0 :initarg :height)
   (channels :initform 0 :initarg :channels)
   (internal-format :initarg :internal-format)
   (min-filter :initform :linear :initarg :min)
   (mag-filter :initform :linear :initarg :mag)
   (reference :initform nil)))

(defun channels-to-format (channels)
  (case channels
    (1 :red)
    (2 :rg)
    (3 :rgb)
    (4 :rgba)
    (otherwise (error (format nil "Unsupported channels value ~a" channels)))))

(defmethod initialize-instance :after ((self gl-texture) &key)
  (with-slots (reference) self
    (setf reference (first (gl:gen-textures 1)))))
;    (gl:bind-texture :texture-2d reference)
;    (gl:tex-parameter :texture-2d :texture-min-filter min-filter)
;    (gl:tex-parameter :texture-2d :texture-mag-filter mag-filter)
;    (gl:tex-image-2d :texture-2d 0 internal-format width height 0 (channels-to-format channels)
;		     :unsigned-byte (cffi:null-pointer))))

(defmethod load-texture-data ((gl-tex gl-texture) (ram-tex texture))
  (with-slots (width height channels internal-format reference min-filter mag-filter) gl-tex
    (with-slots ((w width) (h height) (chn channels) 
		 (min-flt min-filter) (mag-flt mag-filter)
		 (int-fmt internal-format)) ram-tex
      ;;are images the same format?
    (let ((buffer-equal 
	   (and 
	    (eql width w) (eql height h)
	    (eql channels chn)
	    (eql min-filter min-flt)
	    (eql mag-filter mag-flt)
	    (eql internal-format int-fmt))))

      (unless buffer-equal
	(setf width w)
	(setf height h)
	(setf channels chn)
	(setf min-filter min-flt)
	(setf mag-filter mag-flt)
	(setf internal-format int-fmt))

      (let ((data (get-data ram-tex)))
	(gl:bind-texture :texture-2d reference)
	(if buffer-equal
	    (gl:tex-sub-image-2d :texture-2d 0 0 0 width height (channels-to-format channels) 
				 :unsigned-byte data)
	    (progn
	      (gl:tex-parameter :texture-2d :texture-min-filter min-filter)
	      (gl:tex-parameter :texture-2d :texture-mag-filter mag-filter)
	      (gl:tex-image-2d :texture-2d 0 internal-format width height 0 (channels-to-format channels) :unsigned-byte data))))))))

(defmethod load-graphics-object ((self raw-texture) last-version)
  (with-slots (width height channels data internal-format min-filter mag-filter) self
    (unless last-version
	(setf last-version 
	      (make-instance 'gl-texture 
			     :width width 
			     :height height 
			     :min min-filter
			     :mag mag-filter
			     :channels channels
			     :internal-format internal-format))
      (load-texture-data last-version self)
      last-version)))

(defun ptr-to-array (ptr type len)
  (let ((out-array (make-array len :element-type type)))
    (loop for idx from 0 below len do
	 (setf (aref out-array idx) (cffi:mem-ref ptr type idx)))
    out-array))

(defmethod get-data ((tex file-texture))
  (with-slots(path) tex
    (unless (string= path *devil-last-img*)
      (cl-devil:bind-image *devil-img*)
      (cl-devil:load-image path))
    (ptr-to-array (cl-devil:get-data)
		  :unsigned-char
		  (* (cl-devil:image-height) (cl-devil:image-width)
		     (cl-devil:image-bytes-per-pixel)))))
    

(defvar *devil-inited* nil)
(defvar *devil-img* nil)
(defvar *devil-last-img* nil)
(defmethod load-graphics-object ((tex file-texture) last-version)
  (unless *devil-inited*
    (setf *devil-inited* t)
    (cl-devil:init)
    (setf *devil-img* (cl-devil:gen-image)))
  
  (cl-devil:bind-image *devil-img*)
  (with-slots (path width height channels 
		    internal-format min-filter 
		    mag-filter) 
      tex
    (unless last-version
      (setf last-version 
	    (make-instance 'gl-texture :internal-format internal-format)))
  
    (unwind-protect
	 (progn
	   (cl-devil:load-image path)
	   (setf *devil-last-img* path)
	   (setf width (cl-devil:image-width))
	   (setf height (cl-devil:image-height))
	   (setf channels (cl-devil:image-bytes-per-pixel))
	   (load-texture-data last-version tex)))
      last-version))
	       
    
    

	      
	    
	    
	    
      
    
      
    

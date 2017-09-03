(defpackage :window (:use :cl :event :context-manager)
	    (:export :window :get-window-size :make-context-current
		     :destroy-window :in-window :swap-buffers
		     :show-window :hide-window
		     :load-from-context
		     :contexts 
		     
		     ))
(in-package :window)
(defclass window ()
  ((width :initform 100 :initarg :width)
   (height :initform 100 :initarg :height)
   (is-shown :initform nil)
   (screen-x :initform nil :initarg :x)
   (screen-y :initform nil :initarg :y)
   (title :initform "unnamed window" :initarg :title)
   (glfw-window-ref)
   (context-manager)
   (mouse-move-event :initform (make-instance 'event))
   (mouse-click-event :initform (make-instance 'event))
   (cursor-enter-event :initform (make-instance 'event))
   (scroll-event :initform (make-instance 'event))
   (resize-event :initform (make-instance 'event))
   (refresh-event :initform (make-instance 'event))
   (close-event :initform (make-instance 'event))
   (focus-event :initform (make-instance 'event))
   (iconify-event :initform (make-instance 'event))
   (key-event :initform (make-instance 'event))
   (char-event :initform (make-instance 'event))
   ))


(utils:export-class 'window)

(defmethod get-window-size ((win window))
  (multiple-value-bind (width height) 
      (glfw:get-window-size (slot-value win 'glfw-window-ref))
    (list width height)))

(defmethod show-window (win)
  (with-slots ((ref glfw-window-ref) is-shown) win
    (unless is-shown
      (glfw:show-window ref)
      (setf is-shown t))))
  
(defmethod hide-window (win)
  (with-slots ((ref glfw-window-ref) is-shown) win
    (when is-shown
      (glfw:hide-window ref)
      (setf is-shown nil))))

(defun destroy-window (window)
  (progn
    (with-slots (context-manager glfw-window-ref clicked) window
      (glfw:Destroy-Window glfw-window-ref)
      (remove window (slot-value context-manager 'contexts)))))

(defvar +current-context+ nil)

(defmethod make-context-current ((self window))
  (unless (equal +current-context+ self)
    (with-slots (glfw-window-ref) self
      (glfw:Make-Context-Current glfw-window-ref))   
    (setf +current-context+ self)))
  
(defmacro in-window (window &rest body)
  `(let ((old_context +current-context+))
     (make-context-current ,window)
     (unwind-protect
	  
	    (progn ,@body)
	 (progn 
	   (when old_context
	     (make-context-current old_context))
	   (setf +current-context+ nil)))))
     
(defmethod swap-buffers ((self window))
  (with-slots (glfw-window-ref) self
    (glfw:Swap-Buffers glfw-window-ref)))

(defvar +current-windows+ (make-hash-table :test 'equal))

(defvar ++glfw-bind-list (list))
(defmacro ++def-glfw-cb (name args window-event-slot glfw-bind-fun)
  `(progn 
     (cffi:defcallback ,name
	 :void ,(append '((winref :pointer)) args)
	 (let ((window (gethash (cffi:pointer-address winref) +current-windows+)))
	   (with-slots (,window-event-slot) window
	     (invoke ,window-event-slot ,@(mapcar #'first args)))))
     (push (list ',name  #',glfw-bind-fun) ++glfw-bind-list)
	  ))

(setf ++glfw-bind-list '())
(++def-glfw-cb mouse-pos ((x :double) (y :double)) mouse-move-event glfw:Set-Cursor-Pos-Callback)
(++def-glfw-cb mouse-click ((button :int) (action :int)) mouse-click-event glfw:Set-Mouse-Button-Callback)
(++def-glfw-cb cursor-enter ((action :int)) cursor-enter-event glfw:Set-Cursor-Enter-Callback)
(++def-glfw-cb win-scroll ((xscroll :double) (yscroll :double)) scroll-event glfw:Set-Scroll-Callback)
(++def-glfw-cb win-resize ((width :int) (height :int)) resize-event glfw:Set-Window-Size-Callback)
(++def-glfw-cb win-refresh () refresh-event glfw:Set-Window-Refresh-Callback)
(++def-glfw-cb win-close () close-event glfw:Set-Window-Close-Callback)
(++def-glfw-cb win-focus ((action :int)) focus-event glfw:Set-Window-Focus-Callback)
(++def-glfw-cb win-icon ((action :int)) iconify-event glfw:Set-Window-Iconify-Callback)
(++def-glfw-cb win-key ((key :int) (action :int)) key-event glfw:Set-Key-Callback)
(++def-glfw-cb win-char ((key :unsigned-int)) char-event glfw:Set-Char-Callback)

(defmacro reg-glfw-cbs ()
  `(defun reg-callbacks (glfw-win-ref)
     ,(append '(progn) 
	      (mapcar (lambda (x)
			`(funcall ,(second x) glfw-win-ref 
				  (cffi:callback ,(first x))
				  )) 
		      ++glfw-bind-list))))

(reg-glfw-cbs)

(defmethod initialize-instance :after ((self window) &key other-window)
  (with-slots (width height screen-x screen-y glfw-window-ref context-manager clicked title close-event) self
    (if other-window
	(setf context-manager 
	      (slot-value other-window 'context-manager))
	(setf context-manager (make-instance 'context-manager)))
    (glfw:Window-Hint glfw:VISIBLE 0)
    
    (setf glfw-window-ref 
	  (glfw:Create-Window width height title (cffi:null-pointer)
			      (if other-window 
				  (slot-value other-window 'glfw-window-ref) 
				  (cffi:null-pointer))))
    (setf (gethash (cffi:pointer-address glfw-window-ref) +current-windows+) self)
    (reg-callbacks glfw-window-ref)
    (add-listener close-event 
		  (lambda () 
		    (progn (print "closing window..")
			   (destroy-window self))))
    (glfw:Show-Window glfw-window-ref) ;;Should be after set window pos
    (when (and screen-x screen-y)
      (glfw:Set-Window-Pos glfw-window-ref screen-x screen-y))
    (let ((xy (glfw:Get-Window-Pos glfw-window-ref)))
      (setf screen-x (first xy))
      (setf screen-y (second xy)))
    (add-context context-manager self)))

;;Check if gobj is in context. If not or if out of date, call load-graphics-object
(defmethod load-from-context ((self window) (gobj g-object))
  (with-slots (context-manager) self
    (make-context-current self)
    (load-graphics-object-to-context context-manager gobj)))




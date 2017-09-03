(re.load "threading.lisp")
(use-package :reload-engine)
(re.load "reflect.lisp")
(re.load "utils.lisp")
(use-package :reflect)
(re.load "event.lisp")
(use-package :event)
(re.load "glfw3.lisp")
(re.load "context-manager.lisp")
(use-package :context-manager)
(re.load "window.lisp")
(use-package :window)
(re.load "shader-program.lisp")
(use-package :shader-program)
(re.load "vertex-buffer.lisp")
(use-package :vbo)
(re.load "texture.lisp")
;(load "continuation.lisp")
;(reset-reloader)

;;to be implemented later
(defun render (window)
  (sleep 0.5)
  (hide-window window)
  ;(print "...")
  )

(defun mouse-click (button action)
  (print "Default mouse-click implementation")
  )

(defun mouse-wheel (xscroll yscroll)
  ())

(defun mouse-move (x y)
  ;(print "Default mouse-move implementation")
  )

(defun key-action (button action)
  (print "Default key-action implementation")
  )

(defun key-press (key)
  (print "Default key-press implementation")
  )

(defun key-release (key)
  (print "Default key-release implementation")
  )

(defun screen-size-changed(width height)

  )

;;Dispatcher for the graphics thread
(defvar dispatch nil)
(unless dispatch
  (setf dispatch (make-instance 'dispatcher:thread-dispatcher))
  (dispatcher:start-new-dispatcher-thread dispatch :name "Graphics")
  )

(defvar initialized nil)
(defvar win nil)
(defvar render-running t)
(defun do-render ()
  (when render-running
    (restart-case(render win) (skip ()))
    (dispatcher:begin-invoke dispatch #'do-render)))

(defun restart-renderer()
  (setf render-running t)
  (dispatcher:begin-invoke dispatch #'do-render))

(defun stop-renderer()
  (progn
    (setf render-running nil)
))

(defvar key-value-lut (make-hash-table))
(loop for num from 0 to 400 do
     (setf (gethash num key-value-lut)
	   (intern (string (code-char num)))))

(defvar special-keys '(
		       (left 263) (up 265) (right 262) (down 264) 
		       (space 32) (enter 257) (shift 340) (rshift 344)
		       (left-control 341) (right-control 345)(caps-lock 280)
		       (tab 258)(backspace 259)(escape 256) (f1 290)
		       (f2 291)(f3 292)(f4 293)(f5 294)(f6 295)(f7 296)
		       (f8 297)(f9 298)(f10 299)(f11 300)(f12 301)))
(loop for (key val) in special-keys do
     (setf (gethash val key-value-lut) key))

(defvar key-action-lut #(up down repeat))
(defvar mouse-action-lut #(up down))
(defvar mouse-button-lut #(left right middle scroll))

(defun load-events (win)
  (with-slots (scroll-event mouse-move-event mouse-click-event
			    resize-event key-event) win
    (mapcar #'event:clear-listeners (list scroll-event mouse-move-event mouse-click-event
					  resize-event key-event))
    (add-listener scroll-event #'mouse-wheel)
    (add-listener mouse-move-event #'mouse-move)

    (add-listener mouse-click-event
		  (lambda (button action) 
		    (mouse-click 
		     (aref mouse-button-lut button) 
		     (aref mouse-action-lut action))
		    ))

    (add-listener resize-event
		  (lambda (width height)
		    (in-window win
			       (gl:viewport 0 0 width height))
		    (screen-size-changed width height)))

    (add-listener key-event
		  (lambda (key action)
		    (let ((key (gethash key key-value-lut))
			  (action (aref key-action-lut action)))
		      
		      (when (eql action 'down)
			(key-press key))
		      (when (eql action 'up)
			(key-release key))
		      (key-action key action))))))


(defun initialize ()
  (glfw:Init)
  (setf win (make-instance 'window :height 200 :width 200))
  (load-events win)
)

(unless initialized
  (setf initialized t)
  (dispatcher:begin-invoke dispatch #'initialize))

(re.load "package-test.lisp")
(re.load "with-package-macro/with-package.lisp")
(use-package :with-package)
(re.load "cl-enumerate/cl-enumerate.lisp")
(re.load "vector-math2.lisp")
(re.load "cl-reactive/cl-reactive.lisp")

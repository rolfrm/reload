(defpackage :functional-gui (:use :cl)
	    (:nicknames :fungui)
	    (:export :make-window :context :window)
	    )

(in-package :functional-gui)
(defvar *dispatch* nil)
(defvar *initialized* nil)

(defun ensure-dispatch-loaded()
  (unless *dispatch*
    (setf *dispatch* (make-instance 'dispatcher:thread-dispatcher :name "graphics"))
    (dispatcher:begin-invoke *dispatch* (alexandria:curry #'update-window-timeouts *context-manager*))))

(defvar *initialized* nil)

(defstruct window
  (size (m:vec 1 1))
  (title "untitled")
  (timeout 1.0)
  (win-id))

(defun window (win-id &key (size (m:vec 1 1)) (title "Untitled") (timeout 1.0))
  (make-window :size size :title title :timeout timeout :win-id win-id))

(defun make-current (window)
  (with-slots (glfw-ref) window
    (glfw:make-context-current glfw-ref)))

(defmethod funtree:load-element :around 
    ((win window) context child-elements)
  (ensure-dispatch-loaded)
  (unless *initialized*
    (setf *initialized* t)
    (glfw:init))
    
  (flet ((sub-call ()
	   (let ((wind (get-window-holder *context-manager* win)))
	     (with-slots (time-out events glfw-ref last-check ) wind
	       (setf time-out (window-timeout win))
	       (setf last-check (utils:accurate-time))
	       (make-current wind)
	       (with-slots (size) win
		 (glfw:set-window-size glfw-ref (m:ref size 0) (m:ref size 1))
		 (glfw:show-window glfw-ref))
	       (gl:clear :color-buffer-bit)
	       (call-next-method win (utils:add-assoc :window wind context) child-elements)
	       (glfw:swap-buffers glfw-ref)
	       (glfw:poll-events)
	       
		(when events
		  (with-slots (win-id) win
		    (let ((events-copy events))
		      (setf events nil)
		      (cons win-id events-copy))))
		
		))))
    
    (let ((err nil))
      (let ((result 
	     (dispatcher:invoke 
	      *dispatch* 
	      (lambda ()
		(restart-case
		    (sub-call)
		  (skip ()))
					;(error (e) (setf err e))) 
		))))
	(when err
	  (error err))
	result
	))))

(defclass window-holder ()
   ((window :initarg :win)
    (id :initarg :id)
    (last-check :initform (utils:accurate-time))
    (time-out :initform 1.0 :initarg :time-out)
    (glfw-ref :initarg :glfw-ref)
    (context :initform (make-hash-table :test 'equal))
    (events :initform ())
    ))


(defclass context-manager ()
  ((windows :initform (make-hash-table))))

(defun get-items (context-manager)
  (with-slots(windows) context-manager
    (alexandria:hash-table-values windows)))


(defun destroy-window (win-holder context-manager)
  (make-current win-holder)
  (with-slots (glfw-ref id) win-holder 
    (with-slots (windows) context-manager
      (with-slots (context) win-holder
	(let ((err nil))
	  (dispatcher:invoke *dispatch* 
			     (lambda ()
			       
			       (loop for context-item in (alexandria:hash-table-values context) do
				    (handler-case
					(item-manager:clear-items context-item)
				      
				      (error (e) (unless err (setf err e)) )))))
	  (dispatcher:invoke *dispatch* (lambda ()
					  (glfw:destroy-window glfw-ref)))
	  (when err
	    (utils:printf "Error during remove: ~a" err))
	  (remhash id windows )
	  )))))

(defvar *dispatch* nil)
(defvar *context-manager* (make-instance 'context-manager))

(defmethod get-window-holder ((context-manager context-manager) window)
  (with-slots (windows) context-manager
    (with-slots (win-id) window
      (let ((holder (gethash win-id windows)))
	(unless holder
	  (setf holder (create-window-holder window)))
	(setf (gethash win-id windows) holder)
	holder))))

(defun update-window-timeouts (context-manager)
  (with-slots (windows) context-manager
    (handler-case
	(let ((now (utils:accurate-time)))
	  (with-package:with-package enumerate
	     (for-each (alexandria:rcurry #'destroy-window context-manager)
		       (where 
			(lambda (win)
			  (with-slots (last-check time-out) win
			    (> (- now last-check) time-out)))
			(walk-list (alexandria:hash-table-values windows))
			))))
      (error (e) (format nil "Caught error: ~a" e)))
    (sleep 0.01) ;;10ms < 100fps
    (dispatcher:begin-invoke *dispatch* (alexandria:curry #'update-window-timeouts context-manager))))


(defun create-window-holder (window-proto)
  (with-slots (size title win-id) window-proto
    (let ((win-ref (glfw:create-window (m:ref size 0) (m:ref size 1) 
				       title
				       (cffi:null-pointer) (cffi:null-pointer))))
      (let ((output (make-instance 'window-holder :win window-proto :id win-id
				   :time-out 1.0
				   :glfw-ref win-ref)))
	
	(reg-callbacks win-ref)
	(glfw:show-window win-ref)
	output))))

(defclass window-test-thingy ()
  (last-context))

(defmethod funtree:load-element :before ((test-thingy window-test-thingy) context child-elements)
  (with-slots(last-context) test-thingy
    (setf last-context context)))

(defun get-window-holder-from-ref(win-ref)
  (find (cffi:pointer-address win-ref) (get-items *context-manager*)
	:key (lambda (item) (cffi:pointer-address (slot-value item 'glfw-ref)))
	:test 'equal))

(defun load-keys()
  (let ((outp (make-hash-table)))
    (loop for num from 0 to 400 do
	 (setf (gethash num outp)
	       (intern (string (code-char num)) "KEYWORD")))

    (let ((special-keys '(
			   (left 263) (up 265) (right 262) (down 264) 
			   (space 32) (enter 257) (shift 340) (rshift 344)
			   (left-control 341) (right-control 345)(caps-lock 280)
			   (tab 258)(backspace 259)(escape 256) (f1 290)
			   (f2 291)(f3 292)(f4 293)(f5 294)(f6 295)(f7 296)
			   (f8 297)(f9 298)(f10 299)(f11 300)(f12 301))))
    
      (loop for (key val) in special-keys do
	   (setf (gethash val outp) 
		 (intern (symbol-name key) "KEYWORD"))))
    outp))

(defparameter key-value-lut (load-keys))
(defparameter key-action-lut #(:up :down :repeat))
(defparameter mouse-action-lut #(:up :down))
(defparameter mouse-button-lut #(:left :right :middle :scroll))

(defun mouse-button-translate(button-id)
  (aref mouse-button-lut button-id))

(defun mouse-action-translate (action-id)
  (aref mouse-action-lut action-id))
(defun key-value-translate (key-id)
  (gethash key-id key-value-lut))
(defun key-action-translate (action-id)
  (aref key-action-lut action-id))

(defparameter ++glfw-bind-list ())
(defmacro ++def-glfw-cb (args glfw-bind-fun lambda-fcn)
  (let ((name (gensym)))
    `(progn 
       (cffi:defcallback ,name
	   :void ,(cons '(winref :pointer) args)
	 (let ((window-holder (get-window-holder-from-ref winref)))
	   (when window-holder
	     (funcall ,lambda-fcn window-holder ,@(mapcar #'car args)))))
       (push (list ',name #',glfw-bind-fun) ++glfw-bind-list))))

(defun pushevt (window-holder event-name &rest values)
  (with-slots(events) window-holder
    (setf events (cons (cons event-name values) events))))

(setf ++glfw-bind-list '())
(++def-glfw-cb ((x :double) (y :double)) glfw:Set-Cursor-Pos-Callback
	       (lambda (window-holder x y) 
		 (pushevt window-holder :cursor-pos (m:vec x y))))

(++def-glfw-cb ((button :int) (action :int)) glfw:Set-Mouse-Button-Callback
	       (lambda (window-holder button action)
		 (pushevt window-holder :mouse-button (mouse-button-translate button)
						      (mouse-action-translate action))))

(++def-glfw-cb ((action :int)) glfw:Set-Cursor-Enter-Callback 
	       (lambda (window-holder action)
		 (pushevt window-holder :cursor-enter (if (eq 0 action) :outside :inside))))

(++def-glfw-cb ((xscroll :double) (yscroll :double)) glfw:Set-Scroll-Callback 
	       (lambda (window-holder xscroll yscroll)
		 (pushevt window-holder :scroll xscroll yscroll)))

(++def-glfw-cb ((width :int) (height :int)) glfw:Set-Window-Size-Callback 
	       (lambda(win-holder width height)
		 (pushevt window-holder :size width height)))

(++def-glfw-cb () glfw:Set-Window-Refresh-Callback
	       (lambda (win-holder)
		 (pushevt window-holder :close)))

(++def-glfw-cb () glfw:Set-Window-Close-Callback
	       (lambda (win-holder)
		 (pushevt win-holder :close)))

(++def-glfw-cb ((action :int)) glfw:Set-Window-Focus-Callback
	       (lambda (win-holder action)
		 (pushevt win-holder :focus action)))

(++def-glfw-cb ((action :int)) glfw:Set-Window-Iconify-Callback
	       (lambda (win-holder action)
		 (pushevt win-holder :iconify action)))

(++def-glfw-cb ((key :int) (action :int)) glfw:Set-Key-Callback
	       (lambda (win-holder key action)
		 (pushevt win-holder :key (key-action-translate action) (key-value-translate key))))

(++def-glfw-cb ((key :unsigned-int)) glfw:Set-Char-Callback
	       (lambda (win-holder key)
		 (pushevt win-holder :char (code-char key))))

(defmacro reg-glfw-cbs ()
  `(defun reg-callbacks (glfw-win-ref)
     ,(append 
       '(progn) 
       (mapcar 
	(lambda (x)
	  `(funcall ,(second x) glfw-win-ref 
		    (cffi:callback ,(first x))
		    )) 
	++glfw-bind-list))))
(reg-glfw-cbs)


(defun open-window-test()
  (let ((tester (make-instance 'window-test-thingy)))
    (funtree:load-element (funtree:scene-node 
			   (make-window :size (m:vec 200 200) 
					:title "test window" 
					:timeout 1.0)
					      
					      (funtree:scene-node tester tester))
			nil nil)
    (with-slots (last-context) tester
      (assert (eq (length last-context) 1)))
    ))

(test:register 'open-window-test)



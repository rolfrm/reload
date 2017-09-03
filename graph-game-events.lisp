(defclass ui-events ()
  ((key-event :initform (reactive:make-emitter))
   (mouse-button :initform (reactive:make-emitter))
   (mouse-move :initform (reactive:make-emitter))
   (mouse-scroll :initform (reactive:make-emitter))))

;; ;UI/Keyboard
;; (defparameter key-event (reactive:make-emitter))
;; ;UI / mouse
;; (defparameter mouse-button (reactive:make-emitter))
;; ;;ui/mouse
;; (defparameter mouse-move (reactive:make-emitter))
;; ;;Ui/mouse
;; (defparameter mouse-scroll (reactive:make-emitter))
;; ;;UI/mouse core-binding

(defun load-ui-events()
  (make-instance 'ui-events))

(defvar *current-ui-event-handler* (load-ui-events))


(defun set-ui-event-handler(ui-event)
  (setf *current-ui-event-handler* ui-event))

(defun mouse-wheel (scroll-x scroll-y)
  (with-slots (mouse-scroll) *current-ui-event-handler*
    (reactive:invoke mouse-scroll scroll-x scroll-y)))

;;ui/mouse core binding
(defun mouse-click(button action)
  (with-slots(mouse-button) *current-ui-event-handler*
    (reactive:invoke mouse-button button action)))

;;ui/mouse core binding
(defun mouse-move (x y)
  (with-slots (mouse-move) *current-ui-event-handler*
    (reactive:invoke mouse-move (matrix:make-vector (list x y)))))

;;ui/key binding
(defun key-action (key action)
  (with-slots (key-event) *current-ui-event-handler*
    (reactive:invoke key-event key action)))
;;ui/key unused binding
(defun key-press(key) ())
;;ui/key unused binding
(defun key-release (key) ())

;; ;UI / keyboard control / needs to be hidden
;;  ;UI /keyboard
;;   (defun is-down(key)
;;     (or (eq (gethash key key-map) 'repeat) 
;; 	(eq (gethash key key-map) 'down)))




;UI / key handling emitter. More or less generic.. key-listener
(defun key-down (filter-key key-event)
  (reactive:filter key-event 
		   (lambda (key action) 
		     (and (eq key filter-key) 
			  (eq action 'down)))))

;UI / more or less generic mouse handling
(defun load-mouse-events(mouse-move mouse-button drag-threshold filter-button)
  (let* (
	(last-mouse-pos (matrix:make-vector '(0.0 0.0)))
	(mouse-move-delta (reactive:aggregator mouse-move (lambda (fst snd) (matrix:- fst snd))))
	(mouse-left-down (reactive:filter mouse-button
		   (lambda (button action) 
		     (values
		      (and (eq button filter-button) 
			   (eq action 'down)) last-mouse-pos))))

	(mouse-left-up  (reactive:filter mouse-button
		   (lambda (button action) 
		     (values
		     (and (eq button filter-button) 
			  (eq action 'up))
		     last-mouse-pos))))
	)
    (let ((mouse-left-is-down nil)
	  (dragged-distance 0.0)
	  )
      (reactive:subscribe mouse-move 
			  (lambda (pos) (setf last-mouse-pos pos)))

      (reactive:subscribe mouse-left-down 
			  (lambda (pos) 
			    (progn 
			      (setf mouse-left-is-down t) 
			      (setf dragged-distance 0.0))))

      (reactive:subscribe mouse-left-up 
			  (lambda (pos) 
			    (progn 
			      (setf dragged-distance 0.0) 
			      (setf mouse-left-is-down nil))))

      (reactive:subscribe mouse-move-delta 
			  (lambda (motion) 
			    (incf dragged-distance (distance motion))))
      
      (let ((mouse-drag (reactive:filter mouse-move-delta 
					 (lambda (motion) 
					   (values (and mouse-left-is-down 
							(> dragged-distance drag-threshold))
						   motion))))

	    (mouse-click (reactive:filter mouse-left-up
					  (lambda (pos) 
					    (values (< dragged-distance drag-threshold)
						    pos))))
	    )
	(list mouse-drag mouse-click mouse-left-down)))))


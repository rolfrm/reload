
(defvar circ-radius 1) ; sqrt(circ-radius / 2 / PI)
(defvar circ-area 0); (* circ-radius circ-radius PI 2))
(defvar circ-message "")
(defvar _pi 314159/100000)

;; hot, but non-propagating inlets
(defun update-message()
  (let ((newv (format nil "area: ~a" circ-area)))
    (unless (string= circ-message newv)
      (print (setf circ-message newv)))))

(defun set-circ-area (area)
  (unless (equalp circ-area area)
    (setf circ-area area)
    (let ((newv (sqrt (/ circ-area _pi))))
      (set-circ-radius newv))))

(defun set-circ-radius (rad)
  (unless (equalp circ-radius rad)
    (setf circ-radius rad)
    (let ((newv (* circ-radius circ-radius _pi)))
      (set-circ-area newv))
    (update-message)
    ))
(comment
  (defvar f1 (defbinding circ-area circ-radius
	       (* circ-radius circ-radius 2 _pi)))
  (defvar f2 (defbinding circ-radius circ-area
	       (sqrt (/ circ-area _pi 2))))
  
  (set-value circ-radius 1.0)
  
  )

(defstruct binding
  (update-form nil))

(defvar form-lookup (make-hash-table)) 

(defmacro defbinding (set-form get-form update)
  `(flet ((update-value ()
	    (let ((v ,update))
	      (unless (equalp v ,set-form)
		(setf ,set-form v)
		(loop for x in (gethash ,get-form form-lookup) do
		      (funcall (binding-update-form x)))))))
     #'update-value))

(defparameter upd (defbinding circ-area circ-radius (* circ-radius circ-radius _pi)))
(print (funcall upd))
(comment
 (let ((x 1.0)
       (y 1.0))
	 
   (let ((xn (defnode x))
	 (yn (defnode y)))
     (let ((bnd (bind xn yn (* y 2))))
       (set-value yn 3.0))
     )))

(defstruct node
  (get-value nil)
  (set-value nil)
  (binders nil))

(defun node-value (node)
  (funcall (node-get-value node)))

(defun (setf node-value) (value node)
  (funcall (node-set-value node) value)
  (loop for bind in (node-binders node) do
       (funcall bind))
  )


(defmacro defnode (&optional form)
  (if form
      `(make-node :get-value (lambda () ,form)
		  :set-value (lambda (x) (setf ,form x))
		  :binders nil)
      `(let ((backing-field nil))
	 (make-node :get-value (lambda () backing-field)
		    :set-value (lambda (x) (setf backing-field x))
		    :binders nil))))
  

(defmacro bind (node-1 node-2 fcn)
  `(setf (node-binders ,node-2)
	(cons
	 (lambda ()
	   (let ((new-value ,fcn)
		 (current-value (node-value ,node-1)))
	     (unless (equalp new-value current-value)
	       (setf (node-value ,node-1) new-value))))
	 (node-binders ,node-2))))

(let ((n1 (defnode circ-area))
      (n2 (defnode circ-radius))
      (n3 (defnode circ-message))
      (n4 (defnode)))
  (bind n1 n2 (* _pi circ-radius circ-radius))
  (bind n2 n1 (sqrt (/  circ-area _pi) ))
  (bind n3 n1 (format nil "area: ~a" circ-area))
  (bind n4 n3 (format t "'~a' ~%" circ-message))
  (setf (node-value n1) _pi)
  (print (node-value n2))
  (setf (node-value n2) 1)
  ;(print circ-area)
  ;(print circ-radius)
  ;(print circ-message)
  )

(comment

  (defun window (&rest args)
    args)

  (window  :name win
	   :title (bind (format nil "My Window ~a ~a"
				(bind (window-width win))
				(bind (window-height win))))
	   )


  )

(defmacro protected (symbols &rest args)
  (labels (
	 (convert (x)
	   (cadr (assoc x symbols)))

	 (transform-tree (l)
	   
	   (mapcar
	    (lambda (x)
	      (typecase x
		(list (transform-tree x))
		(symbol (convert x))
		(t x))) l)))
    (cons 'progn (transform-tree args))
    ))

#|
(defvar x2 5)
(print (expand-macro (protected ((x x2) (print print) (+ +)
				 (let let))
	 (let ((y 234))
	   (print (+ y y x 10)))))
|#	   
(comment
  ;;;
  ;;; yields are inserted to the
  ;;; nested code feels more like
  ;;; a scripting language
  ;;;
 
 (protected
  (let (x 5)
    (while (< x 100)
      (print (+ x x))
      (setf x (+ x 5))))
  ;; is transformed into
   (let ((x 5))
     (let ((it
	    (lambda ()
	      (print (+ x x))
	      (setf x (+ x 5))
	      (if (< x 100)
		  it
		  (lambda ())))))
       it))
   )

 ;;;
 ;;; But in reality cc is not always really what is needed.
 ;;; sometimes a data oriented style is more what we want
 ;;;
 
 (defcontrol media-control
     :var (is-playing nil)
     :template
     (panel (button :name 'play :tooltip "Press to play" :command 'play-command :is-enabled (not is-playing))
	    (button :name 'stop :tooltip "Press to stop" :command 'stop-command :is-enabled is-playing)
	    )
     :command-handler (play-command (p) (setf is-playing t))
     :command-handler (stop-command (p) (setf is-playing nil))
     )
 (defnode ctrl
     :var (active nil)
     :message (format "~a" active)
     :activator (setf active (not active))
     )
 (defnode ping
     :activator (ping () (blank-command))
     )
 (defnode add-fcn
     :var (v1 0)
     :var (v2 0)
     :input hot-value
     :input (cold-values :rest)

     
     :command-handler (number-command (p idx)
				      (if (eq idx 0)
					  (progn
					    (setf v1 p)
					    (number-command (+ v1 v2)))
					  (setf v2 p)))
     )
 (defnode number
     :var (num 0)
     :command-handler (number-command (p)
				      (setf num p)
				      (number-command num)
				      )
     )
 (defnode printer
     :command-handler ())
 (defnode adder
     (add-fcn :name self
	      :hot-value (number :initial-value 1
				 (ping :name pinger))
	      :cold-values
	      (number :name var :initial-value 0 (node-ref :name self)))
   :activator (push (ping-ping pinger)))

 
 ;; audio example

 (defnode sine-osc
     :active (time (* (sin (* time frequency)) amplitude));; this means it continously generates audio
     :var (frequency 440) ;; the frequency of the oscillation
     :var (amplitude 1) ;; the amplitude of the oscillation
     ) 
 
 (defnode multiplex
     :var (data ()) ;; The selected tones
     :var (template ()) ;; template that controls how tones are generated.
     )

 (multiplex
  :data '(440 550 660 770 880)
  :template (sine-osc :frequency tone :amplitude 1)
  )
 
 
 )

(defclass blank-command ()())
(defclass number-command (blank-command)
  ((x :initform 0)))
(defclass node2 ()())
(defclass ping (node2)())
(defclass number-node (node2)
  ((value :initform 0)))

(defparameter connections (make-hash-table))

(defun get-connections (item)
  (gethash item connections))

(defun send-command (c cmd)
  ())

(defun ping-ping(ping)
  (let ((cmd (make-instance 'blank-command)))
    (let ((connections (get-connections ping)))
      (loop for c in connections do
	   (send-command c cmd)))))

(let ((n1 (make-instance 'ping))
      (n2 (make-instance 'number-node)))
  (setf (gethash n1 connections) (list n2)))



(defun build-node (graph)
  (loop while graph do
       (let ((c (car graph)))
	 (typecase c
	   (keyword (print "keyword"))
	   (list (print "list"))
	   (t (print "other"))))
       (setf graph (cdr graph))))

(defmacro defnode (name &rest args)
  (build-node args)
  `(print ',name))

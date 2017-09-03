(ql:quickload 'cl-cont)
(use-package 'cl-cont)
(ql:quickload 'alexandria)
(use-package 'alexandria)

(defun/cc yeild ()
    (let/cc
	k
      k))

(defun/cc wait-some (time id)
  (let ((end (+ time (get-universal-time))))
    (loop while (> end (get-universal-time)) do
	 (yeild2))
    1))

(defun get-exact-time()
  (multiple-value-bind (seconds microseconds)
      (get-time-of-day)
    (list seconds microseconds)))

(defun/cc test-cont ()
  (let ((ID (random 100)))
    (loop for x from 0 to 100 do
	 (print (format nil "~a ." ID))
	 (yeild)	 
	 (print (format nil "~a .." ID))
	 (yeild)
	 (print (format nil "~a ..." ID))
	 (yeild)
	 (print (wait-some (random 5) ID)))))

(defun cctest()
  (let ((a 0))
    (let ((c1 (test-cont))
	  (c2 (test-cont))
	  (c3 (test-cont))
	  (c4 (test-cont)))
      (loop for x from 0 to 1000 do
	   (sleep 0.1)
	   (setf c1(funcall c1))
	   (setf c2(funcall c2))
	   (setf c3(funcall c3))
	   (setf c4(funcall c4)))
      
      )))

(defclass continuum ()
  ((continuation :initarg :continuum)))

(defmethod continuum-step ((cnt continuum))
  (with-slots (continuation) cnt
      (setf continuation (funcall continuation))))

(defclass game-object-ai()
  ;Active behaviours. Multiple pseudo threads. Can cause interruptions
  ;on the main behaviour
  ((active-behaviours :initform nil)
   (to-delete :initform nil)
   (world :initform nil)))

(defmethod get-world ((game-object-ai game-object-ai))
  (slot-value game-object-ai 'world))

(defmethod set-world ((game-object-ai game-object-ai) world-value)
  (setf (slot-value game-object-ai 'world) world-value))

(defclass lumberjack (game-object-ai)
  ((main-continuum)
  (listen-continuum)))
(defmethod initialize-instance :after ((self lumberjack) &key)
  (with-slots (main-continuum listen-continuum) self
    (setf main-continuum (make-instance 'continuum :continuum (lumberjack-regular self)))
    (setf listen-continuum (make-instance 'continuum :continuum (lumberjack-look-around self)))
    (add-behaviour self main-continuum)
    (add-behaviour self listen-continuum)))

(defmethod step-behaviours ((game-object-ai game-object-ai) world)
  (with-slots (active-behaviours to-delete) game-object-ai
    
    (loop for cc in active-behaviours do
	 (progn
	   (unless (find cc to-delete) ;;Dont use to be deleted..
	     (continuum-step cc))
	   cc))
    (setf active-behaviours (remove-if (rcurry #'find to-delete) active-behaviours))
    (setf to-delete nil)))

(defmethod delete-behaviour ((game-object-ai game-object-ai) behaviour)
  (with-slots (to-delete active-behaviours) game-object-ai
    (push behaviour to-delete)))

(defmethod add-behaviour ((game-object-ai game-object-ai) behaviour)
  (with-slots (active-behaviours) game-object-ai
    (push behaviour active-behaviours)))


(defun/cc wait-awhile(seconds)
  (let ((end (+ (get-universal-time) seconds)))
    (loop while (< (get-universal-time) end) do
	 (yeild))))

(defun/cc go-to (character location)
  (progn
    (print "going to..")
    (wait-awhile 5)
    (print "went..")
    ))

(defun/cc chop-tree (lumberjack tree)
  (progn (print "chopping..")
  (wait-awhile 1)
  (loop for x from 0 to (random 5) collect x)))

(defun/cc pick-up (lumberjack object)
  (progn (print "Picking up")
  (wait-awhile 1)))

(defun/cc put-held-object-in-bag (lumberjack)
  (progn 
    (print "putting in bag")
    (wait-awhile 1)))

(defun/cc look-around-for-nearest-tree (lumberjack)
  (with-slots (target-tree) lumberjack
    (until target-tree
	   (look-around))
    target-tree))
    

(defun find-nearest (world object-type)
  1)

(defun/cc lumberjack-regular (lumberjack)
  (with-slots (world) lumberjack
    (progn
      (print "started")
      (yeild)
      (loop while t do
	   (let ((tree 1));(look-around-for-nearest lumberjack)))
	     (if tree
		 (progn
		   (go-to lumberjack tree)
		   (let ((pieces (chop-tree lumberjack tree)))
		     (loop for x in pieces do
			  (print x)
			  (pick-up lumberjack x)
			  (put-held-object-in-bag lumberjack))
		     ;;No more trees..
		     (wait-awhile 1)))))))))

(defun/cc lumberjack-look-around (lumberjack)
  (with-slots (world) lumberjack
    (progn
      (yeild)
      (loop while t do
	   (loop for x from 0 to 360 by 40 do
		(print (format nil "looking direction ~a" x))
		
		(wait-awhile 2))
	   (print "a spider!")
	   (let ((first-behaviour (first (last (slot-value lumberjack 'active-behaviours)))))
	     (delete-behaviour lumberjack first-behaviour)
	     (wait-awhile 1)
	     (print "killing spider")
	     (wait-awhile 1)
	     (print "bash bash bash!")
	     (wait-awhile 1)
	     (print "*spider dead*")
	     (wait-awhile 1)
	     (delete-behaviour lumberjack (first (slot-value lumberjack 'active-behaviours)))
	     (add-behaviour lumberjack first-behaviour)
	     (yeild))))))
	      

(defun make-lumberjack ()
  (let ((ljack (make-instance 'lumberjack)))
    ljack))

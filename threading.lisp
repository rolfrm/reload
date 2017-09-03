(load "queue.lisp")
(defpackage :dispatcher
  (:use :cl :queue)
  (:export :thread-dispatcher :start :start-new-dispatcher-thread
	   :invoke :stop :begin-invoke :remove-call :wait :result
	   :is-current-thread :interrupt
	   )
  )

(defpackage :future
  (:use :cl)
  (:export :future :result :wait))
(defpackage :task
  (:use :dispatcher)
  (:export :begin-invoke))

(in-package :dispatcher)
(require 'bordeaux-threads)
(require 'trivial-timeout)
(require 'alexandria)


(defmacro await (condition &optional (sleep 0.001))
  `(progn
     (loop until ,condition do
	  (sleep ,sleep))
     t))
(defclass future ()
  ((done :initform nil)
   (output :initform nil)))

(defmethod result ((future future))
  (wait future))

(defmethod wait ((future future))
  (with-slots (done output) future
    (await done)
    output))

(defmethod set-output ((future future) value)
  (with-slots (done output) future
    (setf output value)
    (setf done t)))

(defmethod set-failed ((future future) error)
  (with-slots (done) future
    (setf done t)))

(defclass thread-dispatcher ()
  ((event-queue :initform (make-queue))
   (active :initform nil)
   (keep-alive :initform nil)
   (name :initform "dispatcher" :initarg :name)
   (thread :initarg :thread :initform nil)
   (lock :initform (bt:make-recursive-lock "dispatcher-lock"))
   (idle-time :initform 0.0)))

(defun make-dispatcher ()
  (make-instance 'thread-dispatcher))

(defmethod run ((self thread-dispatcher))
  (with-slots (event-queue active) self
    (unless active
      (setf active t)  
      (unwind-protect
	   (loop while active do

		(loop while (queued? event-queue) do
		     (let ((fcn (deq event-queue)))
		       (restart-case
			   (funcall fcn)
			 (skip ())
			 (re-queue (&optional v) (enq event-queue fcn)))))
		(sleep 0.001))
	(setf active nil)))))

(defmethod start-new-dispatcher-thread 
    ((dispatcher thread-dispatcher) &key (name "Dispatcher"))

  (with-slots (thread name) dispatcher
    (unless (and thread (bt:thread-alive-p thread))
      (setf thread 
	    (bt:make-thread (alexandria:curry #'run dispatcher) 
			    :name name))))
  dispatcher)

(defmethod start ((dispatcher thread-dispatcher) &key (name "Dispatcher"))
  (start-new-dispatcher-thread dispatcher :name name)
  dispatcher)

(defmethod stop ((dispatcher thread-dispatcher))
  (with-slots (active) dispatcher
    (setf active nil)
    dispatcher))

(defmethod interrupt ((dispatcher thread-dispatcher) interruption-fcn)
  (with-slots (thread) dispatcher
    (bt:interrupt-thread thread interruption-fcn)))

(defun future-invoke (function)
  ())

(defmethod begin-invoke ((dispatcher thread-dispatcher) function)
  (with-slots(lock) dispatcher
    (bt:with-recursive-lock-held (lock)
      (start-new-dispatcher-thread dispatcher)
      (with-slots (event-queue active) dispatcher
	(let ((future (make-instance 'future)))
	  (enq event-queue (lambda () 
			     (unwind-protect
				  (set-output future (funcall function))
			       (set-failed future :error))
			     ))
	  future)))))

(defmethod invoke ((dispatcher thread-dispatcher) function)
  (if (is-current-thread dispatcher)
      (funcall function)
      (result (begin-invoke dispatcher function))))

(defmethod is-current-thread((dispatcher thread-dispatcher))
  (with-slots (thread) dispatcher
    (eq thread (bt:current-thread))))

(defun test-dispatcher()
  (let ((dispatcher (make-dispatcher))
	(current-works nil))

    (unwind-protect
	 (wait (begin-invoke dispatcher (lambda () (setf current-works (is-current-thread dispatcher)))))
      (stop dispatcher))
    (unless current-works (error "Is current does not seem to work"))
    ))

(test:register 'test-dispatcher)

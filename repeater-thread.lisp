(defpackage :repeater
  (:use :cl)
  (:export :repeater :start-repeater :stop-repeater :repeater-step))
(in-package :repeater)

(defclass repeater ()
  ((action :initarg :action) 
   (repeat-thread :initform nil)
   (running  :initform nil)
   (sleep-time :initform 0.01 :initarg :sleep-time)))

(defmethod repeater-step ((repeater repeater))
  (with-slots (action sleep-time) repeater
      (funcall action)
      (sleep sleep-time)))

(defmethod repeater-loop ((repeater repeater))
  (with-slots (running) repeater
    (loop while running do (repeater-step repeater))))

(defmethod start-repeater ((self repeater))
    (with-slots (repeat-thread running action sleep-time) self
      (unless running
	(setf running t)
	(unless repeat-thread
	  (setf repeat-thread (bt:make-thread (alexandria:curry #'repeater-loop self) :name "repeater"))))))

(defmacro with-timeout-catched (timeout-seconds body handler)
     `(handler-case
	 (trivial-timeout:with-timeout (,timeout-seconds)
  
	    ,body)
       (trivial-timeout:timeout-error (c)
	   (declare (ignore c))
	 ,handler)))

(defmethod stop-repeater ((repeater repeater) &optional (join-timeout-seconds 100))
  (with-slots (running repeat-thread) repeater
    (when (and running repeat-thread)
      (setf running nil)
      (with-timeout-catched join-timeout-seconds
	(bt:join-thread repeat-thread)
	(progn
	  (print "Join repeater thread timed out: destroying thread")
	  (bt:destroy-thread repeat-thread))) ;;This gives an exception in destroyed tread?
      (setf repeat-thread nil))))
  

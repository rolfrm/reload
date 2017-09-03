(defpackage :reload-engine
  (:use :cl)
  (:export :re.load
	   :file-monitor-thread)
  )
(in-package :reload-engine)

(defvar *first-load* t)
(when *first-load*
  (defun re.load (path &optional rec)
    "Temproary re.load function"
    (load path))
  (setf *first-load* nil))


(re.load "threading.lisp")
(re.load "file-monitor.lisp")
(use-package :dispatcher)
(use-package :file-monitor)

(defvar *watched-files* (make-hash-table :test 'equal))

(defclass reloadable-file ()
  ((path :initarg :path)
   (dependencies :initform ())
   (dependers :initform ())
   (timestamp :initform -1)))

(defvar *file-reloader* nil)

(defmethod +ch-load ((obj reloadable-file))
  (with-slots (dependencies timestamp) obj
    (setf timestamp -1)
    (let ((dep (remove-if-not #'second dependencies)))
      (if dep
	  (mapcar #'+ch-load (mapcar #'first dep))
	  (re-load obj)))))

(defun lisp-file? (pathname)
  (with-slots (type) pathname
    (equal type "lisp")))

(defvar *reload-dispatch* nil)
(defun on-file-changed (filepath)
  (let ((reload-object (gethash (truename filepath) *watched-files*)))
    (when reload-object
      (begin-invoke *reload-dispatch* 
		    (alexandria:curry #'+ch-load reload-object)))))
(defparameter *file-monitor-delay-sec* 0.2)

(defun file-monitor-thread (filtering-fcn file-changed-fcn 
			    &optional 
			      (thread-pause 0.1)
			      (thread-name "reloader thread"))

  (let ((dispatch (make-instance 'thread-dispatcher))
	(reloader (make-instance 'file-dir-monitor :path "."
				 :filtering-function filtering-fcn
				 :file-changed file-changed-fcn)))
    
    (start-new-dispatcher-thread dispatch :name thread-name)
    (labels ((update-reloader ()
	       (unwind-protect
		    (monitor-file-changes reloader))
	       (sleep thread-pause)
	       (begin-invoke dispatch #'update-reloader)))
      (update-reloader))
    (values dispatch reloader)))
		
    

(defun reset-reloader()
  (multiple-value-bind (dispatch reloader)
      (file-monitor-thread #'lisp-file?
			   #'on-file-changed 0.1)
    (setf *reload-dispatch* dispatch)
    (setf *file-reloader* reloader)))

(defvar *reloader-running* nil)
(defun ensure-reloading()
  (unless *reloader-running*
    (setf *reloader-running* t)
    (reset-reloader)))

(defvar *current-subsystem* nil)
(defmethod re-load ((self reloadable-file))
  (with-slots (path dependers timestamp) self
    (let ((prev-state *current-subsystem*)
	  (new-timestamp (file-write-date path)))
      (unless (equal new-timestamp timestamp)
	(setf *current-subsystem* self)
	(print path)
	(unwind-protect
	     (load path)
	  (setf timestamp new-timestamp)
	  (setf *current-subsystem* prev-state))))))

(defun re.load (path &optional (volatile nil))
  "Load and append to list of watched files
    If the code is context sensitive enable volatile"
  (ensure-reloading)
  (setf path (truename path))
  (add-watched-file *file-reloader* path)
  (unless (gethash path *watched-files*)
    (let ((obj (make-instance 'reloadable-file :path path)))
      (setf (gethash path *watched-files*) obj)))
  (let ((obj (gethash path *watched-files*)))
    (when *current-subsystem*
      (with-slots ( dependers ) *current-subsystem*
	(with-slots (dependencies) obj
	  (setf dependencies (remove obj dependencies :key #'first))
	  (setf dependers (remove *current-subsystem* dependers :key #'first))
	  (unless (find *current-subsystem* dependencies :key #'first)
	    (push (list *current-subsystem* volatile) dependencies))
	  (unless (find obj dependers) :key #'first)
	  (push (list obj volatile) dependers))))
    (re-load obj)))

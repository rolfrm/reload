(load "event.lisp")
(error "eeeek dont load this..")
(defclass repeater () 
  ((action :initarg :action)
   (file-changed :initform(make-instance 'event))
   (fw-thread :initform nil)
   (running :initform nil)))

(defmethod start ((fw repeater))
  (if (eq (slot-value fw 'fw-thread) nil)
      (flet ((f () 
	       (unwind-protect
		    (loop while 
			 
			 (slot-value fw 'running) do
			 
			 (funcall (slot-value fw 'action)))
		 (progn
		   (setf (slot-value fw 'running) nil)
		   (setf (slot-value fw 'fw-thread) nil)))))
	(progn
	  (setf (slot-value fw 'running) t)
	  (setf (slot-value fw 'fw-thread) 
		(bt:make-thread #'f :name "fw-thread"))))
	(error "repeater already started..")))

(defmethod stop ((fw repeater) &optional (timeout 100.0))
  (if (eq (slot-value fw 'fw-thread) nil)
      (error "File watcher not started")
      (progn 
	(setf (slot-value fw 'running) nil)
	(bt:join-thread (slot-value fw 'fw-thread))
	(setf (slot-value fw 'fw-thread) nil))))
(defun obj-identity (x) x)

(defun filewatcher-to-string-filter (x)
  (let ((mask (cl-inotify:inotify-event-mask x)))
    (list (cl-inotify:inotify-event-name x) mask)))

(defclass file-watcher ()
  ((evt-action :initarg :action)
   (repeater-thread)
   (notify-token)
   (event-filter :initform #'obj-identity :initarg :event-filter) 
   (path :initarg :path :accessor :path)))

(defmethod initialize-instance :after ((fw file-watcher) &key start)
  (flet ((f ()
	   (let ((evt (cl-inotify:next-event (slot-value fw 'notify-token))))
	     (if (eq evt nil)
		 (sleep 0.01)
		 (funcall (compose (slot-value fw 'evt-action)
				   (slot-value fw 'event-filter))
			  evt 
			  )))))
    (progn
      (setf (slot-value fw 'notify-token) (cl-inotify:make-inotify nil))
      (cl-inotify:watch 
       (slot-value fw 'notify-token) 
       (slot-value fw 'path) :all-events)
      
      (setf (slot-value fw 'repeater-thread)
	    (make-instance 'repeater 
			 :action #'f))
      (start (slot-value fw 'repeater-thread)))))


(defclass file-watcher-event (file-watcher event) () )
(defmethod initialize-instance :after ((fwe file-watcher-event) &key)
  (setf (slot-value fwe 'evt-action)
	(lambda (evt) (invoke fwe evt))))

(defmethod dispose-file-watcher ((self file-watcher))
  (progn
    (stop (slot-value self 'repeater-thread))
    (cl-inotify:close-inotify (slot-value self 'notify-token))))


(defun make-evt-handler (path)
  (let ((listen 1))
    (flet ((f (evt)
	     (if listen
		 (progn
		   (setf listen nil)
		   (handle-evt evt path)
		   (setf listen t)))))
      #'f)))

(defvar load-ready nil)
(defun handle-evt (evt path)
  (let ((name (cl-inotify:inotify-event-name evt))
	(mask (first (cl-inotify:inotify-event-mask evt))))
    (unless name
      (setf name path))
    (if (and (not (eq () (search ".lisp" name)))
	     (eq () (search "#" name)))
	(progn 
	  (if (eq mask :close-write)
	      (progn
		(handler-case
		    (setf load-ready name)
		  (error (e) (print e)))))))))


(defun test-reloader (path live-time)
  (let ((fw (make-instance 'file-watcher :path path :action (make-evt-handler path))))
    (unwind-protect
	 (sleep live-time)
      (dispose-file-watcher fw))
    fw))

(defun update(t2)
  ())

(defun make-update-run(live-time path)
  (let* ((time 0)(repeat 
	 (make-instance 'repeater :action 
			(lambda () 
			  (progn (incf time) 
				 (when load-ready
				   (progn
				     (load load-ready)
				     (setf load-ready nil)
				     ))  
				 (update time) (sleep 0.1))))))
    (unwind-protect 
  
	 (progn (start repeat)
		(test-reloader path live-time))
      (stop repeat))))
			

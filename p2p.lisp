; p2p communication implementation
;Might be improved in the future by using some library
;For now just support LAN with tcp.
(load "/home/rolf/quicklisp/setup.lisp")
(ql:quickload 'alexandria)
(use-package 'alexandria)
(ql:quickload 'usocket)
(ql:quickload 'bordeaux-threads)

(defpackage :p2p (:use :common-lisp :alexandria :bt :usocket))
(in-package :p2p)

(defclass p2p-client ()
  ((server-socket :initarg :socket)
   (accept-thread)
   (handle-thread)
   (ip :initarg :ip)
   (port :initarg :port)
   (listen-func :initform (lambda (p2p peer data) (print (format nil "std recieve: ~a ~a ~a" p2p peer data))) :initarg :listen-callback)
   (running :initform t)
   (peers :initform nil)))

(defstruct p2p-peer (socket nil))

(defmethod serialize ((peer p2p-peer))
    (apply #'list 'peer (multiple-value-bind (ip port)
			    (usocket:get-peer-name (p2p-peer-socket peer))
			  (list ip port))))

(defun create-p2p (&optional (ip usocket:*wildcard-host*) (port usocket:*auto-port*) 
		     (listen-func (lambda (p2p peer data) (print (format nil "std recieve: ~a ~a ~a" p2p peer data)))))
  (let ((socket (usocket:socket-listen ip port :reuseaddress t)))
    (setf ip (usocket:get-local-address socket))
    (setf port (usocket:get-local-port socket))
    (print (format nil "~a ~a" ip port))
    (let ((p2p (make-instance 'p2p-client :ip ip :port port :socket socket)))
      (with-slots (accept-thread handle-thread) p2p
	  (setf accept-thread (bt:make-thread (curry #'accept-method p2p) :name "p2p accept"))
	  (setf handle-thread (bt:make-thread (curry #'handle-method p2p) :name "p2p handle")))
      p2p)))

(defmethod accept-method ((cli p2p-client))
  (with-slots (running server-socket peers) cli 
    (loop while running do
	 (handler-case
	     
		   (let ((client-socket (usocket:socket-accept server-socket)))
		     
		     (push (make-p2p-peer :socket client-socket) peers))
	   (condition (e) (print e))))))

(defmethod handle-active-stream ((cli p2p-client) socket)
  "Private method: Handles a single writing stream"
  (let ((stream (usocket:socket-stream socket)))
    (handler-case
	(let ((incomming-data (read stream)))
	  (with-slots (peers listen-func) cli
	    (let ((peer (find socket peers :key #'p2p-peer-socket)))
	      (if peer
		  (bt:make-thread (curry listen-func cli peer incomming-data) :name "handle active-stream")
		  (print "Error: no such peer..")))))
	(condition   (e) (print e)))))
		

(defmethod handle-method ((cli p2p-client))
  (with-slots (peers running) cli
    (loop while running do
	 (if (null peers)
	     (sleep 0.1)
	     (handler-case
		 (let ((active-streams (usocket:wait-for-input (mapcar #'p2p-peer-socket peers) :timeout 0.1 :ready-only t)))
		   ;(print (mapcar (rcurry #'slot-value 'usocket:state) active-streams))
		   (mapcar 
		    (lambda (active-stream)
		      (handle-active-stream cli active-stream))
		    active-streams))
	       (condition (e) (print e)))))))


(defmethod connect ((cli p2p-client) ip port)
  (let ((socket (usocket:socket-connect ip port :nodelay t :timeout 0.1)))
    (let ((new-peer (make-p2p-peer :socket socket)))
      (with-slots (peers) cli
	(push new-peer peers))
      new-peer)))

(defmethod disconnect ((cli p2p-client) (peer p2p-peer))
  (handler-case
      (usocket:socket-close (p2p-peer-socket peer))
    (error () (print "error while closing socket.:"))))

(defmethod close-connection ((cli p2p-client))
  (progn
    
    (mapcar (curry #'disconnect cli) (slot-value cli 'peers))
    (usocket:socket-close (slot-value cli 'server-socket))
    (with-slots (running handle-thread accept-thread) cli
      (setf running nil)
      (sleep 0.1)
      ;(bt:join-thread handle-thread)
      (bt:destroy-thread handle-thread)
      (bt:destroy-thread accept-thread))))

(defmethod send ((cli p2p-client) (peer p2p-peer) data)
  (let ((stream (usocket:socket-stream (p2p-peer-socket peer))))
    (print data stream)
    (force-output stream)))
;;;Use example

(defun buffered-reader()
  (let ((lst (list)) 
	(mtex (bt:make-lock))
	(balance 0))
    (labels ((readf (p2p peer data)
	       (bt:with-lock-held (mtex) 
		 (progn 
		   (print (serialize peer))
		   (incf balance) 
		   (push data lst))))
	     (wait-for-data ()
	       (progn
		 
		 (loop for x from 0 to 100 while (null lst) do
			(sleep 0.01)) 
		 (if (null lst)
		     (error "read timed out..")
		     (bt:with-lock-held 
			 (mtex) 
		       (progn 
			 ;(print "write")
			 (decf balance) 
			 ;(print balance) 
			 (pop lst)))))))
      (values #'readf #'wait-for-data))))
		      

(defun test1 (p)
  (let (( cli1 nil))
    (handler-case
	(progn
	  (setf cli1 (create-p2p #(127 0 0 1) p))
	  (sleep 1.0)
	  (close-connection cli1)
	  )
      (error (e) (print e)))
    cli1))
(defun example-of-use(times)
  (let (cli1 cli2)
    (unwind-protect
	 (multiple-value-bind (readf waitf)
	     (buffered-reader)
	   (progn
	     (setf cli1 (create-p2p))
	     
	     (setf cli2 (create-p2p))
	     (setf (slot-value cli2 'listen-func) readf)
	     (let ((cli2-peer (connect cli1 "localhost" (slot-value cli2 'port))))
	       (loop for x from 0 to times do
		    (send cli1 cli2-peer (format nil "hello ~a" x))
		    (handler-case
			(print (format nil "received ~a"(funcall waitf)))
		      (error (e) (print e)))
		    (send cli1 cli2-peer "hello2")
		    (handler-case
			(print (format nil "received ~a"(funcall waitf)))
		      (error (e) (print e)))))
	     (print "done..")
	     ))
      (progn
	(handler-case
	    (close-connection cli1)
	  (error () ()))
	(handler-case
	    (close-connection cli2)
	  (error () ()))))))
	  
  

(let ((pack (find-package :p2p)))
  (do-all-symbols (sym pack) (when (eql (symbol-package sym) pack) (export sym))))
(in-package cl-user)

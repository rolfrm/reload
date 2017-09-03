(load "/home/rolf/quicklisp/setup.lisp")
(ql:quickload 'alexandria)
(use-package 'alexandria)
(ql:quickload 'usocket)
(ql:quickload 'bordeaux-threads)

(defun test-server2 (ip port object)
  (let ((sock (usocket:socket-listen ip port :reuseaddress t)))
    (unwind-protect
	 (let ((sock2 (usocket:socket-accept sock)))
	   (let ((stream (usocket:socket-stream sock2)))
	     (print object stream)
	     (finish-output stream)
	     (print "done server")
	     (print "..")))
      (usocket:socket-close sock))))

(defun test-client2 (ip port)
  (let ((sock (usocket:socket-connect ip port :nodelay t :timeout 0.1))
	(output))
    (unwind-protect
	 (let ((stream (usocket:socket-stream sock)))
	   (setf output (read stream))
	   (force-output stream)
	   )
      (usocket:socket-close sock))
    output))
    


(defun test-client (serverip port data)
  (let ((sock (usocket:socket-connect serverip port :nodelay t)))
    (let ((stream (usocket:socket-stream sock)))
      (unwind-protect
	   (progn
	     (print (read stream))
	     (print data stream)
	     (force-output stream)
	     (print "client reading..")
	     (sleep 1.0)
	     (print (read stream)))
      (print "not closed")))))
	   

(defmethod serialize (any-object)
  any-object)

(defmethod serialize ((lst list))
  (mapcar #'serialize lst))

(defmethod serialize ((fnc function))
  (multiple-value-bind (lmbd closures name)
      (function-lambda-expression fnc)
    (if lmbd
	lmbd
	(list 'func name))))

(defmethod serialize ((htb hash-table))
  (list 'hashtable (hash-table-plist htb) (hash-table-test htb)))

(defmethod deserialize (anydata)
  anydata)

(defmethod deserialize ((data list))
  (if (null data)
      '()
      (let ((fst (first data)))
	(case fst
	  (func (eval (list 'function (second data))))
	  (hashtable (plist-hash-table (second data) :test (third data)))
	  (lambda (eval data))
	  (otherwise (mapcar #'deserialize data))))))

;;;distribution control

(defclass peer ()
  ((cached-objects :initform ())))

(defclass tcp-peer (peer)
  ((ip :initarg :ip)
   (port :initarg :port)
   (socket :initarg :socket)))

(defclass virtual-object ()
  ((cache :initarg :object)
   (valid :initform t)
   (dist-control :initform (error "must supply distributor") :initarg :distcontrol)
   (name :initform (error "must supply object name") :initarg :name)))

;a object that maintains a distributed package
 (defclass distribution-central ()
  ((package-name :initform 'cl-user :initarg :package-name)
   (peers :initform ())
   (peer->cache :initform ()) 
   (objects :initform '())
   (ip :initarg :ip)
   (port :initarg :port)
   (socket)
   (running :initform t)
   (accept-thread)
   (handle-thread)
   (object-peer-table :initform (make-hash-table))))

(defmethod serialize ((peer tcp-peer))
  (with-slots (ip port) peer
    (list ip port))) 

(defun find-peer (peer-list ip port)
  (first (remove-if-not (curry #'eq port) (remove-if-not (curry #'eq ip) peer-list :key (rcurry #'slot-value 'ip)) :key (rcurry #'slot-value 'port))))


(defmethod server-accept-thread ((central distribution-central))
  (with-slots (peers running socket) central
    (loop while running do
	 (print "accepting connection..")
	 (let ((sock (usocket:socket-accept socket)))
	   (let (;(peer-name (usocket:get-peer-name sock))
		 (peer-ip (usocket:get-peer-address sock))
		 (peer-port (usocket:get-peer-port sock)))
	     (push (make-instance 'tcp-peer :port peer-port :ip peer-ip
				  :socket sock) peers)
	     (let ((stream (usocket:socket-stream sock)))
	       (print "socket ready" stream )
	       (force-output stream)
	       )
	     )))))
(defun unfold-peers (peers)
  (mapcar #'serialize peers))

(defmethod server-get-output ((central distribution-central) message)
  (with-slots (objects) central
    (case (first message)
      (get-peers (unfold-peers peers))
      (get-object-names (mapcar (lambda (x) (slot-value x 'name)) objects))
      (get-object
       (serialize 
	(find (second incomming-data) objects 
	      :key (lambda (x) (slot-value x 'name)))))
      (otherwise 'unknown))))

(defmethod server-handle-active ((central distribution-central) peer-socket)
  (with-slots (objects) central
  (progn 
    (let ((stream (usocket:socket-stream peer-socket)))
      (let ((incomming-data (read stream)))
	(unless (listp incomming-data)
	  (setf incomming-data (list incomming-data)))
	(print (server-get-output central incomming-data) stream)
	(force-output stream))))))

(defmethod server-wait-thread ((central distribution-central))
  (with-slots (peers running) central
    (loop while running do
	 (if (null peers)
	     (sleep 0.01)
	     (let ((active-streams 
		    (usocket:wait-for-input 
		     (mapcar (lambda (peer) 
			       (slot-value peer 'socket)) peers) 
		     :timeout 1000 :ready-only t)))
	       (loop for active-stream in active-streams do 
		    (server-handle-active central active-stream))
	       )))))
	   
(defmethod start-distribution-sync ((central distribution-central))
    (with-slots (ip port socket accept-thread handle-thread) central
	(progn
	  (setf socket (usocket:socket-listen ip port :reuseaddress t))
	  (let (
		(handle-func (curry #'server-wait-thread central))
		(accept-func (curry #'server-accept-thread central)))
	    (setf accept-thread (bt:make-thread accept-func :name "dist. accept"))
	    (setf handle-thread (bt:make-thread handle-func :name "dist. wait"))
	    ))))

(defmethod stop-distribution-sync ((central distribution-central))
    (with-slots (running accept-thread handle-thread) central
      (setf running nil)
      (mapcar #'bt:join-thread (list accept-thread handle-thread))))

(defmethod initialize-instance :after ((central distribution-central) &key)
  (with-slots (package-name) central
    (progn
      (unless (find-package package-name)
	(make-package package-name)))))

(defmethod start-sync ((central distribution-central) ip port)
  ())

(defmethod add-object ((central distribution-central) (virt virtual-object))
  (with-slots (peers) central)
  ())

(defmethod sync-with ((central distribution-central) ip port)
;  (let ((sock (usocket:socket-connect ip port)))
;    (unwind-protect
;	 (with-slots (peers) central
;	   (let ((existing-peer (find-peer peers ip port)))
;	     (unless existing-peer
;	       (setf existing-peer (make-instance 'tcp-peer :ip ip :port port))
;	       (push existing-peer peers)))))))
  (error "not implemented"))

(defmethod sync-object ((central distribution-central) (virt virtual-object))
  (with-slots (valid) virt ;;To be implemented
    (setf valid t)))

(defmethod upload-object ((central distribution-central) (virt virtual-object)) ;;Spawn new thread to mark other values invalid
  ())

(defmethod new-virt ((central distribution-central) object symb)
  (with-slots (objects) central
    (let ((virt (find symb objects :key (rcurry #'slot-value 'id))))
      (if virt
	  (set-virt virt object)
	  (progn
	    (setf virt (make-instance 'virtual-object :name symb :distcontrol central :object object))
	    (push virt objects)))
      virt)))

(defun make-outside-distrib (ip port)
  (declare (ignore ip port))
;  (let ((sock (usocket:socket-connect ip port)))
 ;   (unwind-protect
;	 (let ((stream (usocket:socket-stream sock)))
;	   (write 'get-pkg :stream stream)
;	   (let ((pkg-name (read :stream stream)))
;	     (let ((new-central (make-instance 'distribution-central :package-name pkg-name)))
;	       (sync-with new-central ip port)
;	       new-central)))
 ;     (usocket:socket-close sock))))
(error "Not implemented"))

(defmethod get-virt ((virt virtual-object))
  (with-slots (valid cache dist-control) virt
    (if valid
	cache
	(progn (sync-object dist-control virt)
	       (get-virt virt)))))

;set, then invalidate objects (some kind of lock is needed)
(defmethod set-virt ((virt virtual-object) value)
  (with-slots (cache dist-control valid) virt
    (setf cache value)
    (setf valid t)
    (upload-object dist-control virt)))
;    (unless valid ;;valid changed
 ;     (resolve-conflict virt))))

(defmethod resolve-virt-conflict ((virt virtual-object))
  ())

(load "p2p.lisp")


(defclass server ()
  (
   (p2p-client)
   (uniqueid :initform 0)
   (ip :initarg :ip)
   (port :initarg :port)
   (cache-file :initform nil)
   (servers :initform nil)
   (loaded-objects :initform (make-hash-table :test 'equal))
   (messages :initform nil)
   (msg-lock :initform bt:make-lock)
   (max-cache-size :initform 1000000)
   )
)

(defstruct object-id
  (creator-id)
  (object-id)
)

(defclass node ()
  (
   (valid :initform nil) ;is the current cache valid?
   (cache :initform nil) ;stored data.
   (id :initarg :id) ;unique object id
   (holders :initform nil) ;who holds a cached value?
   )
  )

(defmethod init-peers ((serv server) (first-peer p2p:p2p-peer))
  ())

(defun connect (ip port &optional (local-ip usocket:*wildcard-host*)
			  (local-port usocket:*auto-port*) )
  "Creates a server by connecting to one. Random port can be used"
  (let (server (make-instance 'server :ip ip :port port))
    (let (first-peer 
	  (connect (slot-value server 'p2p-client) ip port))
      (init-peers server first-peer))))

(defun create-server (ip port)
  "Creates the first server"
  (let ((server (make-instance 'server :id 0 :ip ip :port port)))
    (start-server server)
     server))

(defmethod queue-message ((serv server) msg)
  (with-slots (messages msg-lock) serv
    (bt:with-lock-held (msg-lock)
      (push msg messages))))

(defmethod wait-for-message((serv server) &optional predicate)
  ())*COMPILE-VERBOSE*

(defmethod server-get-output((serv server) data)
  "Private method: Gets the server output given some data"
  (with-slots (servers) serv
    (if (listp data)
	'unknown-list
	(case data
	  (peers (serialize (get-servers serv)))
	  (otherwise 'unknown)))))

(defmethod handle-data ((serv server) p2p-cli peer data)
  "Handles the data that needs to be used right away. Rest is put into a list"
  (with-slots (p2p-client) serv
    (with-slots (peers) p2p-cli
      
      (if (listp data)
	  "data list.."
	  (case data
	    (send p2p-cli peer (list 'peers (mapcar #'p2p:serialize peers))))
	    ))))

(defmethod start-server ((server server))
  "Private method. starts server listening, also handles peers connecting"
  (with-slots (ip port p2p-client) server
    (setf p2p-client (p2p:create-p2p ip port 
				     (curry #'handle-data server)))))
      
(defmethod disconnect ((serv server))
  "Signals to peers that this server is now inactive. Dangerous if it has cached objects"
  (with-slots (p2p-client) serv
    (p2p:close-connection p2p-client)))

(defmethod find-node ((serv server) (node-id integer))
  "Find a node with the given id. Locally or externally."
  ;1. Ram. 2. Cache file 3. peers
  (with-slots (loaded-objects) serv
    (let ((from-hash (gethash node-id loaded-objects)))
      (if from-hash
	  from-hash
	  (error "Object not found"))))) 

(defmethod find-node ((server server) (node-name string))
  "Find the named node. Locally or externally"
  ())

(defmethod create-node ((server server) data &keys (name nil))
  "Creates a new node on the server. If no name, the node can only be found by id"
  ;create local node. Only register it locally.
  ())

(defmethod update-node ((server server) (node node) data)
  "Updates a node with the given data. Invalidates on peers"
  ;set the value of a node and inform holders it is invalid
())

(defmethod expand-node ((server server) (node node))
  "Expand a virtual node with data"
  ())

(defmethod forget-node ((server server) (node node))
  "Forgets the data of a node. Makes it virtual"
  ())

(defmethod node-forgetable ((server server) (node node))
  "Test if a node is forgetable. That is if others have it."
  ())

(defmethod cache-node ((server server) (node node))
  "Caches a node to the harddrive. Use serialization for this. Use file system or virtual file system like tar"
  ())

(defun example-of-use()
  (let ((serv-1 (create-server #(127 0 0 1) 9000)))
    (let ((serv-2 (connect #(127 0 0 1) 9000)))
      (let ((node1 (create-node serv-1 123 :name "origin")))
	(let ((node1-2 (find-node serv-2 "origin")))
	  (expand-node serv-2 node1-2) ;;Expand -> caching data
	  (update-node serv-2 node1-2 (list (create-node serv-2 444)))))  ;;update value
      (disconnect serv-2)
      (disconnect serv-1))))

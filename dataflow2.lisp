(defclass node ()
  ((sub-nodes :initform nil)))

(defmethod print-object ((object node) stream)
    (format stream "n"))

(defparameter node-plugins nil)

(defun register-node-plugin (name fcn)
  (push (list name fcn) node-plugins))


(defvar var-tables (make-hash-table))
(defmacro gethash-or-create (key table value)
  (let ((keysym (gensym))
	(tablesym (gensym)))
    `(let ((,keysym ,key)
	   (,tablesym ,table))
    (let ((v (gethash ,keysym ,tablesym)))
       (unless v
	 (setf v ,value)
	 (setf (gethash ,keysym ,tablesym) v))
       v))
    ))
       

(defun get-var-table(name)
  (gethash-or-create name var-tables (make-hash-table :weakness :key)))

(defvar obj-vars (make-hash-table :weakness :key))

(defun nodevar (node name)
  (let ((tab (get-var-table name)))
    (gethash node tab)))

(defun (setf nodevar) (value node name)
  (let ((tab (get-var-table name)))
    (setf (gethash node tab) value)))


(defmacro comment (&rest args)
  (declare (ignore args))
  ())


(defmacro defnode (name &rest args)
  (loop while args do
       (let ((c (car args)))
	 (setf args (cdr args))
	 (typecase c
	   (keyword
	    (let ((kw c) (arg (car args)))
	      (setf args (cdr args))))
	   (list (print c))))))

	      

(defun var-kw (node arg)
  (destructuring-bind
	(name initial) arg
      (setf (nodevar node name) initial)))

(register-node-plugin :var #'var-kw)

(comment
  (defnode number
      :var (num 1))
  (defnode adder
      :var (result 0)
      )

  (defnode test1
      (adder
       (number :num 1)
       (number :num 3)))
  
  ;; how to bind numbers to result in adder automatically?
  
  (let ((test-node (make-node test1)))
    (assert (eq (adder-result test-node) 4))))

(defclass number-node (node)
	   ((num :initform 0 :initarg :num)))

(defclass adder-node (node)
  ((result :initform 0 :initarg :result)))

(defun make-number(&key (num 1))
  (make-instance 'number-node :num num ))

(defun make-adder(&key (result 0))
  (make-instance 'adder-node :result result))

(defclass test1 (node)())

(defun make-test1()
  (let ((node (make-instance 'test1)))
    (let ((adder (make-adder))
	  (number1 (make-number :num 1))
	  (number2 (make-number :num 2)))
      (setf (slot-value adder 'sub-nodes) (list number1 number2))
      (setf (slot-value node 'sub-nodes) (list adder)))
    node))

(defun loaded(context node)
  (print context))

(defun emit-cmd(context cmd)
  ())

(defun load-graph(root-node)
  (labels ((load-graph2 (ctx node)
	     (loaded ctx node)
	     (let ((sub-nodes (node-sub-nodes node))
		   (ctx2 (cons node ctx)))
	       (loop for node in sub-nodes do
		    (load-graph2 ctx2 node)))))
    (load-graph2 nil root-node)))


(defun make-node (node-kind &rest args)
  ())


;; this stuff. 
(comment
  (defnode number
      :var (num 1))
  (defnode adder
      :var (result 0)
      )

  (defnode test1
      (adder
       (number :num 1)
       (number :num 3))
    (defvar tst (make-test1))
    (update tst) ;?? nothing can really be done with this circuit as there is 
    ))

  ;; should turn into:
(defun run-test1()
  (let ((a 1)
	(b 3)
	(result 0))
    (labels (
	     (update-result ()
	       (setf result (+ a b)))
	     (set-a (v)
	       (setf a v)
	       (update-result))
	     (set-b (v)
	       (setf b v)
	       (update-result)))
      (update-result)
      result)))
	     
(comment
 (defnode number
     :var (num 0 'integer))
 (defnode twoop
     :var (result1 0 'integer)
     :var (result2 0 'integer)
     :update (setf result1 (+ outputs))
     :update (setf result2 (- outputs)))
 (defnode test2
     (twoop
      (number :var 2)
      (number :var 3))))

(defun run-test2(x y)
  (let ((a 0)
	(b 0)
	(result1 0)
	(result2 0))
    (declare (type fixnum a b result1 result2))
    (labels ((update-result1 () (setf result1 (+ a b)))
	     (update-result2 () (setf result2 (- a b)))
	     (set-a (v) (unless (eq a v) (setf a v) (update-result1) (update-result2)))
	     (set-b (v) (unless (eq b v) (setf b v) (update-result1) (update-result2)))
	     )
      
      ;(update-result1)
					;(update-result2)
      (set-a x)
      (set-b y)
      (list result1 result2))))
      
	     
	
	      
    
	       
	       
	      

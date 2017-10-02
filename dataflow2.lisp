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
(comment
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
)
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
     :var (num 0)
     ;; update can be specified to push a result.
     :update (() num)
     :update ((x) (setf num x) num)
     ) 
 (defnode twoop
     " This node has two outputs: a + b and a - b "
     (number :num (apply #'+ args))
     (number :num (apply #'- args))
     )
 ;; since twoop has two outputs you have to refer to
 ;; to the outputs by name
 (defnode printer
     :var (format "~a")
     :update ((x) (format nil format x)))
(defnode group)
 
(defnode test2
    :var (x 0)
    :var (y 0)
    (printer
     :format "result 1: ~a")
    (twoop :name twoop
	   (number :var x)
	   (number :var y))  
    (printer
     :format "result 2: ~a"
     (bind twoop :result2) )
    )
 )

(declaim (optimize (safety 0) (speed 3) (debug 0) (space 3)))
(defstruct nodedef
  (inputs nil :type list)
  (outputs nil :type list))
(defun run-test2()
  (let (
	;; define variables for each node
	(a 0)
	(b 0)
	(result1 0)
	(result2 0)

	;; defines outputs
	(output1 nil)
	(output2 nil)
	)
    (declare (type fixnum a b result1 result2)
	     (type list output1 output2))
    (labels (
	     (print-result1 () (format t "result 1: ~a ~%" result1))
	     (print-result2 () (format t "result 2: ~a ~%" result2))
	     (update-result1 () (setf result1 (+ a b)) (print-result1) (loop for x in output1 do (funcall x result1)))
	     (update-result2 () (setf result2 (- a b)) (print-result2) (loop for x in output2 do (funcall x result2)))

	     ;; declare input methods
	     (set-x (v) (declare (type fixnum v)) (set-a v))
	     (set-y (v) (declare (type fixnum v)) (set-b v))
	     
	     (set-a (v) (declare (type fixnum v)) (unless (and nil (eq a v)) (setf a v) (update-result1) (update-result2)))
	     (set-b (v) (declare (type fixnum v)) (unless (and nil (eq b v)) (setf b v) (update-result1) (update-result2)))
	     (add-output1 (op) (setf output1 (cons op output1)))
	     (add-output2 (op) (setf output2 (cons op output2)))
	     )
      (make-nodedef :inputs (list #'set-x #'set-y)
		    :outputs (list #'add-output1 #'add-output2)))))
;;lets wire n1s two outputs to the inputs of n2:
(comment
 (defnode test3
     (group
      (test2 :name n1)
      (test2 :name n2 :x (node-ref n1 0) :y (node-ref n1 1))))

 )

(defun run-test3 ()
  (let ((n1 (run-test2))
	(n2 (run-test2)))
    (funcall (first (nodedef-outputs n1)) (first (nodedef-inputs n2)))
    (funcall (second (nodedef-outputs n1)) (second (nodedef-inputs n2)))
    (make-nodedef :inputs (nodedef-inputs n1)
		  :outputs (nodedef-outputs n2))))
(defun concat-symbols(&rest syms)
  (intern (apply #'concatenate 'string (mapcar #'symbol-name syms))))

(defvar node-definitions (make-hash-table))

(defmacro defnode(name &rest args)
  (progn
    (setf (gethash name node-definitions) args)
    `(defun ,(concat-symbols 'make- name) ()
       (let (
	     ;; vars
	     
	     ;; local-vars
	     
	     ;; outputs
	     
	     )
	 (labels
	     (
	      ;; set-vars

	      ;; updaters

	      ;;
	      )
	   (make-nodedef :inputs () :outputs ()))))))


	       
	      

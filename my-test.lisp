(declaim (optimize (debug 3) (speed 0) (space 0) (safety 3)))
;;; Examples
;;; (deftable my-x-y-points x :i32 y :i32)
;;; (defvector my-vec-xy x :i32 y :i32)
;;; (defvar pts (my-x-y-points-create "optional-name"))
;;; (let ((index (alloc pts 1)))
;;;   (set-value index :x 1 :y 2)
;;;   (print index))
;;;

(in-package :sb-posix)
(print "sb-posix")
(define-call ("mremap") sb-sys:system-area-pointer
  (lambda (res) (print res)
    (= (sb-sys:sap-int res) #.(1- (expt 2 sb-vm::n-machine-word-bits))))
  (addr sap-or-nil) (old-size size-t) (new-size size-t) (flag unsigned)
  (new-addr sb-sys:system-area-pointer))

(defconstant mremap-maymove 1)
(defconstant map-32bit #x40)
(export 'mremap-maymove)
(export 'map-32bit)
(export 'mremap)

(in-package :cl-user)

;(defmacro deftable (name &rest args)
;  (print `',args))

;(deftable my-x-y-points x single-float y single-float)

(defun rel-path (a)
  (let ((dir-part (pathname-directory a))
	(name-part (file-namestring a)))
    (if name-part
	(append (if dir-part dir-part '(:relative)) (list name-part))
	dir-part)))

(defun combine-path (a b)
  (let ((_a (rel-path a))
	(_b (rel-path b)))
    (append _a (cdr _b))))

(defun combined-path (a b)
  (let ((full-path (combine-path a b)))
    (let ((file-name (pathname (car (last full-path)))))
      (make-pathname :directory (butlast full-path) :name (pathname-name file-name) :type (pathname-type file-name)))))

(defvar data-directory (the string "data"))
(declaim (type string data-directory))

(defstruct mem-area
  (ptr (sb-sys:int-sap 0) :type sb-sys:system-area-pointer)
  (size 0 :type fixnum)
  (fd 0 :type fixnum))

(defun mem-create (name &optional only-32bit)
  (declare (type string name)
	   (type boolean only-32bit))
  (let ((full-path (combined-path data-directory name)))
    (ensure-directories-exist (namestring full-path))
    (let ((full-name (namestring full-path)))
      (let ((fd (sb-posix:open full-name (+ sb-posix:o-rdwr sb-posix:o-creat) #o666)))
	(let ((s (sb-posix:lseek fd 0 sb-posix:seek-end)))
	  (sb-posix:lseek fd 0 sb-posix:seek-set)
	  (make-mem-area
	   :ptr (sb-posix:mmap (sb-sys:int-sap 0) 1
			       (+ sb-posix:prot-write sb-posix:prot-read)
			       (+ sb-posix:map-shared (if only-32bit sb-posix:map-32bit 0)) fd 0)
	   :size s
	   :fd fd
	   )))
	)))

(defun mem-realloc(mem-area new-size)
  (declare (type fixnum new-size)
	   (type mem-area mem-area))
  (let ((old-size (mem-area-size mem-area)))
    (when (> new-size old-size)
      (let ((ptr (sb-posix:mremap (mem-area-ptr mem-area) old-size new-size sb-posix:mremap-maymove (sb-sys:int-sap 0))))
	(setf (mem-area-ptr mem-area) ptr)
	(setf (mem-area-size mem-area) new-size)
					;(when (> new-size old-size)
	(sb-posix:ftruncate (mem-area-fd mem-area) new-size))))
    mem-area)

(defun mem-destroy(mem-area)
  (declare (type mem-area mem-area))
  (sb-posix:munmap (mem-area-ptr mem-area) (mem-area-size mem-area))
  (setf (mem-area-ptr mem-area) (sb-sys:int-sap 0))
  (setf (mem-area-size mem-area) 0)
  (sb-posix:close (mem-area-fd mem-area))
  (setf (mem-area-fd mem-area) 0))
  
(defun mem-update(mem-area)
  (declare (type mem-area mem-area))
  (let ((s (sb-posix:lseek (mem-area-fd mem-area) 0 sb-posix:seek-end)))
    (sb-posix:lseek (mem-area-fd mem-area) 0 sb-posix:seek-set)
    (when (> s (mem-area-size mem-area))
      (mem-realloc mem-area s)))
  mem-area)

(defstruct table-base
  (header nil :type mem-area)
  (column-size nil :type cons)
  (column-type nil :type cons)
  )

(defun table-create(name column-sizes column-types)
  (let ((header-table (mem-create (format nil "~a.~a" name "header"))))
    (mem-realloc header-table 8)
    (make-table-base
     :header header-table
     :column-size column-sizes
     :column-type column-types)))
  
(defun table-destroy(table-base)
  (mem-destroy (table-base-header table-base)))

(defparameter table-test (table-create "my-test" '(4) '(i32)))

(defstruct float-map
  (area nil :type mem-area))

(defun float-map-create(name)
  (let ((mem-area (mem-create name)))
    (make-float-map :area mem-area)))

(defun float-map-enlarge(float-map new-size)
  (mem-realloc (float-map-area float-map) (* new-size 4))
  float-map)
 
(defun float-map-at (float-map index)
  (declare (type fixnum index)
	   (type float-map float-map))
  (sb-sys:sap-ref-single (mem-area-ptr (float-map-area float-map)) index))

(defun (setf float-map-at) (value float-map index)
  (declare (type single-float value)
	   (type fixnum index)
	   (type float-map float-map))
  (setf (sb-sys:sap-ref-single (mem-area-ptr (float-map-area float-map)) index) value))

(defun float-map-count (float-map)
  (/ (mem-area-size (float-map-area float-map)) 4))


(defparameter fmap (float-map-create "test.floats"))
(print (float-map-count fmap))
(float-map-enlarge fmap 8)
(setf (float-map-at fmap 0) 1.0)
(print (float-map-at fmap 0))

(setf (float-map-at fmap 1) 2.0)
(print (float-map-at fmap 1))

(setf (float-map-at fmap 2) 3.0)
(print (float-map-at fmap 2))
(setf (float-map-at fmap 7) 3.14)
(print (float-map-at fmap 7))

(defun mmap-example()
  (let ((fd (sb-posix:open "tes4.x" (+ sb-posix:o-rdwr sb-posix:o-creat) #o666)))
    (let ((mmapptr (sb-posix:mmap (sb-sys:int-sap 0) 1024 (+ sb-posix:prot-write sb-posix:prot-read)  sb-posix:map-shared fd 0)))
      (print mmapptr))
    (sb-posix:close fd)))

(defun mem-test-moved-areas()
  (let ((mema (mem-create "my-mem")))
    (mem-realloc mema 1024)
    (setf (sb-sys:sap-ref-64 (mem-area-ptr mema) 0) 10)
    (let ((memb (mem-create "my-mem")))
      (print (sb-sys:sap-ref-64 (mem-area-ptr memb) 0))
      (mem-realloc memb (* 1024 1024))
      (setf (sb-sys:sap-ref-64 (mem-area-ptr memb) 0) 10)
      )))

(defvar circ-radius 1) ; sqrt(circ-radius / 2 / PI)
(defvar circ-area 0); (* circ-radius circ-radius PI 2))
(defvar circ-message "")
(defvar _pi 314159/100000)

;; hot, but non-propagating inlets
(defun update-message()
  (let ((newv (format nil "area: ~a" circ-area)))
    (unless (string= circ-message newv)
      (print (setf circ-message newv)))))

(defun set-circ-area (area)
  (unless (equalp circ-area area)
    (setf circ-area area)
    (let ((newv (sqrt (/ circ-area _pi))))
      (set-circ-radius newv))))

(defun set-circ-radius (rad)
  (unless (equalp circ-radius rad)
    (setf circ-radius rad)
    (let ((newv (* circ-radius circ-radius _pi)))
      (set-circ-area newv))
    (update-message)
    ))
(comment
  (defvar f1 (defbinding circ-area circ-radius
	       (* circ-radius circ-radius 2 _pi)))
  (defvar f2 (defbinding circ-radius circ-area
	       (sqrt (/ circ-area _pi 2))))
  
  (set-value circ-radius 1.0)
  
  )

(defstruct binding
  (update-form nil))

(defvar form-lookup (make-hash-table)) 

(defmacro defbinding (set-form get-form update)
  `(flet ((update-value ()
	    (let ((v ,update))
	      (unless (equalp v ,set-form)
		(setf ,set-form v)
		(loop for x in (gethash ,get-form form-lookup) do
		      (funcall (binding-update-form x)))))))
     #'update-value))

(defparameter upd (defbinding circ-area circ-radius (* circ-radius circ-radius _pi)))
(print (funcall upd))
(comment
 (let ((x 1.0)
       (y 1.0))
	 
   (let ((xn (defnode x))
	 (yn (defnode y)))
     (let ((bnd (bind xn yn (* y 2))))
       (set-value yn 3.0))
     )))

(defstruct node
  (get-value nil)
  (set-value nil)
  (binders nil))

(defun node-value (node)
  (funcall (node-get-value node)))

(defun (setf node-value) (value node)
  (funcall (node-set-value node) value)
  (loop for bind in (node-binders node) do
       (funcall bind))
  )


(defmacro defnode (&optional form)
  (if form
      `(make-node :get-value (lambda () ,form)
		  :set-value (lambda (x) (setf ,form x))
		  :binders nil)
      `(let ((backing-field nil))
	 (make-node :get-value (lambda () backing-field)
		    :set-value (lambda (x) (setf backing-field x))
		    :binders nil))))
  

(defmacro bind (node-1 node-2 fcn)
  `(setf (node-binders ,node-2)
	(cons
	 (lambda ()
	   (let ((new-value ,fcn)
		 (current-value (node-value ,node-1)))
	     (unless (equalp new-value current-value)
	       (setf (node-value ,node-1) new-value))))
	 (node-binders ,node-2))))

(let ((n1 (defnode circ-area))
      (n2 (defnode circ-radius))
      (n3 (defnode circ-message))
      (n4 (defnode)))
  (bind n1 n2 (* _pi circ-radius circ-radius))
  (bind n2 n1 (sqrt (/  circ-area _pi) ))
  (bind n3 n1 (format nil "area: ~a" circ-area))
  (bind n4 n3 (format t "'~a' ~%" circ-message))
  (setf (node-value n1) _pi)
  (print (node-value n2))
  (setf (node-value n2) 1)
  ;(print circ-area)
  ;(print circ-radius)
  ;(print circ-message)
  )

(comment

  (defun window (&rest args)
    args)

  (window  :name win
	   :title (bind (format nil "My Window ~a ~a"
				(bind (window-width win))
				(bind (window-height win))))
	   )


  )

(defmacro protected (symbols &rest args)
  (labels (
	 (convert (x)
	   (cadr (assoc x symbols)))

	 (transform-tree (l)
	   
	   (mapcar
	    (lambda (x)
	      (typecase x
		(list (transform-tree x))
		(symbol (convert x))
		(t x))) l)))
    (cons 'progn (transform-tree args))
    ))

#|
(defvar x2 5)
(print (expand-macro (protected ((x x2) (print print) (+ +)
				 (let let))
	 (let ((y 234))
	   (print (+ y y x 10)))))
|#	   
(comment
  ;;;
  ;;; yields are inserted to the
  ;;; nested code feels more like
  ;;; a scripting language
  ;;;
 
 (protected
  (let (x 5)
    (while (< x 100)
      (print (+ x x))
      (setf x (+ x 5))))
  ;; is transformed into
   (let ((x 5))
     (let ((it
	    (lambda ()
	      (print (+ x x))
	      (setf x (+ x 5))
	      (if (< x 100)
		  it
		  (lambda ())))))
       it))
   )

 ;;;
 ;;; But in reality cc is not always really what is needed.
 ;;; sometimes a data oriented style is more what we want
 ;;;
 
 (defcontrol media-control
     :var (is-playing nil)
     :template
     (panel (button :name 'play :tooltip "Press to play" :command 'play-command :is-enabled (not is-playing))
	    (button :name 'stop :tooltip "Press to stop" :command 'stop-command :is-enabled is-playing)
	    )
     :command-handler (play-command (p) (setf is-playing t))
     :command-handler (stop-command (p) (setf is-playing nil))
     )
 (defnode ctrl
     :var (active nil)
     :message (format "~a" active)
     :activator (setf active (not active))
     )
 (defnode ping
     :activator (ping () (blank-command))
     )
 (defnode add-fcn
     :var (v1 0)
     :var (v2 0)
     :command-handler (number-command (p idx)
				      (if (eq idx 0)
					  (progn
					    (setf v1 p)
					    (number-command (+ v1 v2)))
					  (setf v2 p)))
     )
 (defnode number
     :var (num 0)
     :command-handler (number-command (p)
				      (setf num p)
				      (number-command num)
				      )
     )
 (defnode printer
     :command-handler ())
 (defnode adder
     (add-fcn :name self
	      (number :initial-value 1
		      (ping :name pinger))
	      (number :name var :initial-value 0 (node-ref :name self)))
   :activator (push (ping-ping pinger)))
 (let ((node (create-adder)))
   (adder-push node))
 
 )

(defclass blank-command ()())
(defclass number-command (blank-command)
  ((x :initform 0)))
(defclass node2 ()())
(defclass ping (node2)())
(defclass number-node (node2)
  ((value :initform 0)))

(defparameter connections (make-hash-table))

(defun get-connections (item)
  (gethash item connections))

(defun send-command (c cmd)
  ())

(defun ping-ping(ping)
  (let ((cmd (make-instance 'blank-command)))
    (let ((connections (get-connections ping)))
      (loop for c in connections do
	   (send-command c cmd)))))

(let ((n1 (make-instance 'ping))
      (n2 (make-instance 'number-node)))
  (setf (gethash n1 connections) (list n2)))



(defun build-node (graph)
  (loop while graph do
       (let ((c (car graph)))
	 (typecase c
	   (keyword (print "keyword"))
	   (list (print "list"))
	   (t (print "other"))))
       (setf graph (cdr graph))))

(defmacro defnode (name &rest args)
  (build-node args)
  `(print ',name))


;(set-circ-radius 1)
;(set-circ-radius 2)
;(set-circ-area 1)
;(set-circ-area circ-area)
;(print PI)
(comment
  (defvector my-vector
      (x :type single-float)
      (y :type single-float))
  (defvar my-vector-1 (my-vector-create "table1"))
  (defvar index0 (my-vector-alloc my-vector-1))
  (defvar index1 (my-vector-alloc my-vector-1 4))
  (setf (my-table-x my-table-1 index0) 1.0)
  (setf (my-table-x my-table-1 index1) 0.0 1.0 0.0 1.0)
  (setf (my-table-y my-table-1 index1) 0.0 0.0 1.0 1.0)
  (setf (my-table-xy my-table-1 index1) 0.0 0.0 1.0 0.0 0.0 1.0 1.0 1.0)
  (print (my-table-y my-table-1 index0)) ;; prints '1.0'
  (print (my-table-y my-table-1 index1)) ;; prints '(0.0 1.0 0.0 1.0)'
  
  (deftable my-table (index :type fixnum) (x :type single-float) (y :type single-float))
  (defvar my-table-1 (my-table-create "table2"))
  (defvar index2 (my-table-insert my-table-1 :index 1 :x 3.0))
  (my-table-insert-at my-table-1 :index index2 :x 4.0 :y 5.0)
  (print (my-table-x my-table-1 index2)) ;prints 4.0

  ;; this has to be possible:
  (deftable my-table-2 (index :type string) (x :type fixnum))
  (defvar my-table-3 (my-table-2-create "table3"))
  (my-table-insert my-table-3 :index "value.1" :x 5)

  ;; 
  ;; GUI programming
  ;;
  
  (window .title "hello world"
          .style (style
		  (template 'text :foreground "red")
		  )
    (button .click (lambda (evt) (print "clicked"))
	    (text .text "click me!")))

  ;; what is this really:
  (A .prop-1 "hello world" .name a-ins
     .prop-2 (style (template 'B .prop-3 (color "red")))
     (B .prop-4 (lambda (evt) (print "Clicked!"))
	(C .prop-5 (binding .prop1 :source a-ins))))

  
   
  
  )



  
  
    

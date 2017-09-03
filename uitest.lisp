
(defun combine-symbols (&rest args)
  (intern (apply #'concatenate 'string (mapcar #'symbol-name args)))) 

(defclass graph-object ()
  ((__bind :initform nil)
   (parent :initform nil)))

(defclass A ()
    ((a :initform nil) 
     (__bind :initform nil)))

(defmethod a-changed ((self A) old-value new-value)
  (print (format nil "a changed from ~a to ~a" old-value new-value)))

(defclass B ()
  ((b :initform (make-instance 'A)) 
   (c :initform nil)
   (__bind :initform '((c (b a))))))

(defmethod c-changed ((self B) old-value new-value)
  (print (format nil "c changed from ~a to ~a" old-value new-value)))
(defvar c (make-instance 'B))

(defun set-value (target slot &rest args)
  (if (equal (length args) 1)
      (progn 
	(setf (slot-value target slot) (first args)) ;; First set value
	(funcall (combine-symbols slot '-changed) target (first args) 0)
	(loop for bnd in (slot-value target '__bind) do ;;look for bindings
	     (when (equal (first bnd) slot)
	       (apply #'set-value target (print (append (second bnd) args)))
	       (print (format nil "found binding ~a" bnd)))))
      
      (apply #'set-value (slot-value target slot) (first args) (rest args))))

(defun get-value (target slot &rest slots)
  (progn
    (print (format nil "~a ~a" slot slots))
    (if slots
	(apply #'get-value (slot-value target slot) (first slots) (rest slots))
	(slot-value target slot))))
      

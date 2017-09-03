
;;Utilities:
(defun add-last (lst elem)
  (append lst (list elem)))

(defmacro push-front (elem lst)
  `(setf ,lst (add-last ,lst ,elem)))

(defun take-while-split(predicate sequence)
  (let ((first nil) (second sequence))
    (loop while (and second (funcall predicate (first second))) do
	 (setf first (add-last first (first second)))
	 (setf second (rest second)))
    (values first second)))

(defun printf (format-string &rest rest)
  (let ((first-element (first rest)))
    (print (apply #'format nil format-string rest))
    first-element))

(defmacro print-values (subform)
  `(let ((lst (multiple-value-list ,subform)))
    (printf "values: ~a" lst)
    (apply #'values lst)))

(defun concat-symbols(sym1 sym2)
  (intern (concatenate 'string (symbol-name sym1) (symbol-name sym2))))

;;

(defvar *special-forms* (make-hash-table :test 'eql))
(defvar *cps-functions* (make-hash-table :test 'eql))

(defmacro defun-cc (name args body)
  (progn
    (setf (gethash name *cps-functions*) t)
    (multiple-value-bind
	  (cps-form sym)
	(cps-tform body nil)
      (let ((lmb-form 
	     (if (functionp cps-form)
		 (funcall cps-form sym)
		 body)))
	`(defun ,name ,args ,lmb-form)))))



(defun scope-call (f1 f2)
 (let ((out (multiple-value-list (funcall f1))))
   (if (functionp (first out))
       (values-list 
	(append (list 
		 (lambda () (scope-call (first out) f2))) 
		(rest out)))
       (apply f2 out))))

(defun apply-chaining (form dirty-funcalls)
  (let ((func-name (first form))
	(args (rest form))
	(inner-form nil)
	(outer-form nil))
    (push-front func-name inner-form)
    (let ((cps-args (mapcar (lambda (arg) (multiple-value-list (cps-tform arg dirty-funcalls))) args)))
      (loop for (func-or-form return-form) in cps-args do
	   (if (functionp func-or-form)
	       (progn 
		 (push-front return-form inner-form)
		 (push-front func-or-form outer-form))
	       (push-front func-or-form inner-form)))
      (if outer-form 
	  (let ((sym (gensym)))
	    (values 
	     (lambda (next-form)
	       (let ((new-inner (copy-list inner-form)))
		 
		 (setf new-inner `(let ((,sym ,new-inner))
			,next-form))
		 (loop for f in (reverse outer-form) do
		      (setf new-inner (funcall f new-inner)))
		 new-inner))
	     sym))
	  inner-form))))

(defun cps-tform (form context)
  (progn
    
    (unless (and (listp form) (eql (first form) 'lambda))
      (setf form (macroexpand-1 form)))
    (if (listp form)
	(let ((call-name (first form)))
	  (if (eql call-name 'declare)
	      1
	  (let ((out 
		 (multiple-value-list
		  (cond
		    (
		     (gethash call-name *special-forms*) 
		     (funcall (gethash call-name *special-forms*) form context))
		    (t (apply-chaining form context))))))
	    (let ((found (find call-name (get-dirty-functions context))))
	      (if found
		  (call>cps (first out) context)
		  (values (first out) (second out)))
	      ))))
	form)))

(defun is-dirty (cps-form)
  (if (listp cps-form)
      (functionp (first cps-form))
      nil))

(defun test-dirty ()
  (progn
    (print 1)
    (print 2)
    (let ((a 3))
      (values 
       (lambda ()
	 (print a))
       a))))

(defun any-dirty (cps-form-list)
  (some #'is-dirty cps-form-list))


(re.load "cps-context.lisp")
(re.load "special-transforms.lisp" t)

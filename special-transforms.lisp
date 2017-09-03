(defun yield>cps (form dirty-funcalls)
    (if (rest form)
	(let ((sym (gensym)))
	  (multiple-value-bind (lmb sym2)
	      (cps-tform (second form) dirty-funcalls)
	    (if (functionp lmb)
		(values
		 (lambda (next-form)
		   (funcall lmb 
			    `(values 
			      (lambda () ,next-form)
			      ,sym2)))
		 sym2)
		(values 
		 (lambda (next-form)
		   `(let ((,sym ,lmb))
		      (values (lambda () ,next-form)
			      ,sym))) 
		 sym))))
	(values 
	 (lambda (next-form) `(lambda () ,next-form))
	 nil)))

(defun let>cps (form dirty-funcalls)
  (let ((cps-tformed (mapcar (lambda (x) (multiple-value-list (cps-tform x  dirty-funcalls))) (cddr form))))
    (let ((cps-tformed-args (mapcar (lambda (x)(multiple-value-list (cps-tform (second x)  dirty-funcalls))) (second form))))
      (let 
	  ((newargs (mapcar
		     (lambda (arg form) (list (first arg) form))
		     (second form)
		     (mapcar (lambda (x) (if (eql (length x) 2) (second x) (first x))) cps-tformed-args)))
	   (form-funcs (mapcar #'first (remove-if-not (compose #'functionp #'first  ) cps-tformed-args))))
	
	(let ((is-dirty (loop for (func return) in cps-tformed do (when (functionp func) (return-from nil t)))))
	  (if (and (equal newargs (second form)) (not is-dirty))
	      form
	      (let ((retsym (gensym)))
		(values
		 (lambda (inner-form)
		   (funcall 
		    (apply #'compose (lambda (x) x) form-funcs) 
		    `(scope-call
		      
		      (let ,newargs
			(lambda ()
			  
			  ,(labels 
			    ((rec-tform 
			      (cps-form &optional retform2 )
			      (multiple-value-bind (non-dirty dirty)
				  (take-while-split 
				   (compose #'not #'functionp #'first) cps-form)
				(let ((next-form 
				       (let 
					   ((last 
					     (if dirty 
						 (funcall 
						  (first (first  dirty)) 
						  (let ((ret 
							 (rec-tform 
							  (rest dirty)
							  (second (first dirty)))) )
						    (if ret
							ret
							
							(second (first dirty)))))
						 nil))
					    (collected 
					     (loop for x in non-dirty collect 
						  (first x))))
					 (if last (add-last collected last) 
					     collected))))

				  (let ((next-form-length (length next-form)))
				    (case next-form-length
				      (0 nil)
				      (1 (first next-form))
				      (otherwise
				       (list* 'progn next-form 
					      )))
					       
				    )))))
			    (rec-tform cps-tformed)
			    )))
			      (lambda (,retsym)  ,inner-form))))
		 retsym))
	      ))))))

(defun unroll-progn (cps-form)
  (let ((sym (gensym)))
    (values
    (lambda (x)
      (let ((is-ended nil))

      (labels 
	  ((rec-tform 
	       (cps-form &optional retform2 )
	     (multiple-value-bind (non-dirty dirty)
		 (take-while-split 
		  (compose #'not #'functionp #'first) cps-form)
	       (let ((next-form 
		      (let 
			  ((last 
			    (if dirty 
				(funcall 
				 (first (first  dirty)) 
				 (let ((ret 
					(rec-tform 
					 (rest dirty)
					 (second (first dirty)))) )
				   (if ret
				       ret
				       
				       (second (first dirty)))))
				nil))
			   (collected 
			    (loop for x in non-dirty collect 
				 (first x))))
			(if last (add-last collected last) 
			    collected))))
	   
		 (let ((next-form-length (length next-form)))
		   (let ((ret-form 
			  (case next-form-length
			    (0 nil)
			    (1 (first next-form))
			    (otherwise
			     (list* 'progn next-form 
				    )))))
		     (if is-ended
			 ret-form
			 (progn
			   (setf is-ended t)
			  
			   `(let ((,sym ,(if ret-form ret-form retform2)))
			      ,x)))
		   ))))))
  
	(rec-tform cps-form)
	)))
    sym)))

(defun last-element(lst)
  (first (last lst)))

(defun tagbody-unroll (restform)
  (multiple-value-bind (lists rest)
      (take-while-split #'listp (rest restform))
    (let ((sym (first restform))
	  (nextsym (first rest))
	  (body lists))
      (if (and nextsym (not (equal 'go (first (last-element lists)))))
	  (setf body (add-last body (list 'go nextsym))))
	  
      (if rest
	  (list* (list sym body) (tagbody-unroll rest))
	  (list (list sym body))))))

(defun tagbody-replace-go-calls(forms)
  (mapcar 
   (lambda (form) 
     (if (listp form)
	 (if (equal (first form) 'go) 
	     (list (second form))
	     form)
	 form))
   forms))

(defun tagbody>labels(form  dirty-funcalls)
  (let ((realform (tagbody-replace-go-calls (rest form))))
	 
    (multiple-value-bind (init rest)
	(take-while-split #'listp (rest form))
      (let ((unrolled (tagbody-unroll rest)))
	(setf init (add-last init (list (first (first unrolled)))))
	`(labels ,(mapcar 
		   (lambda (x) 
		     (list (first x) 
			   '() 
			   (list* 'progn 
				  (tagbody-replace-go-calls  
				   (first
				    (rest x)))))) 
		   unrolled)
	 ,@(tagbody-replace-go-calls init)
	 )))))

(defun labels>cps (labels-form tform-context)
  (let ((label-funcs (second labels-form))
	(implicit-progn (cddr labels-form))
	(inner-context tform-context))
    (dotimes (n-unused (length label-funcs))
      (let ((label-cps-forms-pre 
	     (mapcar 
	      (lambda (sub-label-form)
		(let ((label-name (first sub-label-form))
		      (label-arg (second sub-label-form))
		      (label-form (car (cddr sub-label-form))))
		  (list label-name
			(multiple-value-bind (tformed return-sym)
			    (cps-tform label-form inner-context)
			  (if  (functionp tformed)
			      'dirty
			      'non-dirty)))))
	      label-funcs)))
	(loop for dirty-cps in (remove-if-not (lambda (x) (equal (second x) 'dirty)) label-cps-forms-pre) do
	     (unless (find (first dirty-cps) (get-dirty-functions inner-context))
	       (setf inner-context (add-to-dirty-functions inner-context (first dirty-cps)))))))
      
    (let ((label-cps-forms 
	   (mapcar 
	    (lambda (sub-label-form)
	      (let ((label-name (first sub-label-form))
		    (label-arg (second sub-label-form))
		    (label-form (car (cddr sub-label-form))))
		(list label-name label-arg
		      (multiple-value-bind (tformed return-sym)
			  (cps-tform label-form inner-context)
			  (if (functionp tformed)
			      (list 'dirty (funcall tformed return-sym))
			      (list 'non-dirty label-form))))))
	    label-funcs)))
      (if (some (lambda (x) (equal (first (third x)) 'dirty)) 
		label-cps-forms)
	  
	  (let ((arg-forms 
		   (loop for sub-form in label-cps-forms collect
			(let ((name (first sub-form))
			      (arg (second sub-form))
			      (form (second (third sub-form)))
			      (is-dirty (first (third sub-form))))
			  (list name arg form))))
		(cps-impl-progn
		 (case (length implicit-progn)
		   (0 ())
		   (1 (multiple-value-list (cps-tform (first implicit-progn) inner-context)))
		   (otherwise (multiple-value-list (cps-tform (list* 'progn implicit-progn) inner-context))))))
	    (if (and (listp cps-impl-progn)(functionp (first cps-impl-progn)))
		(values
		 (lambda (form)
		   (list 'labels arg-forms (funcall (first cps-impl-progn) form)))
		 (second cps-impl-progn))
		(list 'labels arg-forms cps-impl-progn)))
	  labels-form)
      )))

(defun progn>cps (form context)
  (let ((cps-forms (mapcar (lambda (subform) (multiple-value-list (cps-tform subform context))) (rest form))))
    (if (not (any-dirty cps-forms))
	form
	(case (length cps-forms)
	  (1 (let ((single-val (first cps-forms))) 
	       (values 
		(first single-val) 
		(second single-val))))
	  (otherwise
	   (unroll-progn cps-forms))))))

(defun call>cps (funcform context)
  (let ((sym (gensym))
	(cps-tformed (mapcar (lambda (subform) (multiple-value-list (cps-tform subform context))) (rest funcform))))
    (values
     (lambda (next-form)
       (list 'scope-call
	     `(lambda () ,funcform)
	     `(lambda (,sym) ,next-form)))
     sym)))

(defun tagbody>cps (form context)
  (let ((labels (tagbody>labels form context)))
    
    (labels>cps labels context)))

(defmacro tst-tagbody()
	   (multiple-value-bind
	       (form sym) 
	       (cps-tform 
		'(yield 
		  (tagbody 
		     (print 1) 
		   _a 
		     (print 'x2) 
		     (yield 1) 
		   _b 
		     (print 'y) 
		   _c 
		     (yield 1) 
		     (go _b)
		     )) nil)
	     (if (functionp form)
		 (funcall form sym)
		 form)
	   ))
(defvar scope-exit-sym (gensym))
(defun block>cps (form context)
  (let ((tag (second form))
	(sub-forms (list* 'progn (cddr form))))
    (let ((is-dirty (functionp (cps-tform sub-forms context))))
      (setf context (add-to-block-scope context (list tag is-dirty)))
      (if is-dirty
	  (multiple-value-bind (form2 ret-form)
	      (cps-tform sub-forms context)
		(values form2 ret-form))
	  form))))

(defun return-from>cps (form context)
  (let ((tag (second form))
	(value 
	 (let ((trd (third form)))
	   (if (eql trd :value)
	       (fourth form)
	       (third form)))))
    (let ((block-scope-value (find tag (get-block-scope context) :key #'first)))
      (if block-scope-value
	  (if (second block-scope-value)
	      (let ((sym (gensym)))
		(values (lambda (form) `(let ((,sym ,value))
					  ,form))
			sym))
	      form)
	  form))))
	      
      
(defun test-block()
  (progn
    (block>cps '(block a (print a) (return-from a)) nil)
    (return-form>cps '(return-from nil :value 10) nil)))

(defun n-times (n func)
  (when (> n 0)
    (funcall func n)
    (n-times (- n 1) func)))

(defmacro loop-while (test-form run-form)
  (let ((_a (gensym)) (_b (gensym)) (_c (gensym)))
  `(tagbody
      ,_a 
      (if ,test-form
	  (go ,_b)
	  (go ,_c))
      ,_b
       ,run-form
       (go ,_a)
      ,_c)))

(defun lambda>cps(form context)
  (let ((expr (first(cddr form)))
	(args (cadr form)))
    (multiple-value-bind (cps-form ret-sym)
	(cps-tform expr context)
      (if (functionp cps-form)
	  `(lambda ,args ,(funcall cps-form ret-sym))
	  form))))

(defun funcall>cps(form context)
  (multiple-value-bind
	(form sym)
      (apply-chaining form context)
    (let ((return-sym (gensym)))
      (values
      (lambda (sub-form)
	(if (functionp form)
	    
	    (funcall form `(scope-call ,sym (lambda () ((lambda (,return-sym) ,sub-form)))))
	    `(scope-call (lambda() ,form) (lambda (,return-sym) ,sub-form))))
      return-sym))))

(defun apply>cps(form context)
  (multiple-value-bind
	(form sym)
      (apply-chaining form context)
    (let ((return-sym (gensym)))
      (values
      (lambda (sub-form)
	(if (functionp form)
	    
	    (funcall form `(scope-call ,sym (lambda () ((lambda (,return-sym) ,sub-form)))))
	    `(scope-call (lambda() ,form) (lambda (,return-sym) ,sub-form))))
      return-sym))))
		

(defmacro let-list (symbols values &rest body)
  (let ((syms symbols) (listsym (gensym)))
    
    (let ((get-values nil))
      (loop while syms do
	   (let ((sym (pop syms)))
	     (if (eq sym '&rest)
		 (push (list (pop syms) (quote listsym)) get-values)
		 (push (list sym (list 'pop listsym)) get-values)))) 
      `(let ((,listsym ,values))
	 (let* ,(reverse get-values)
	   ,@body)))))

(defun cpsListToForm(cps-list)
  (if (functionp (first cps-list))
      (funcall (first  cps-list) (second cps-list))
      (first cps-list)))
  
(defun if>cps (form context)
  (let ((cond-var (gensym)))
    (let-list 
     (form-name cond-form if-true-form if-false-form)
     form
     (let-list
      (cps-cond cps-true cps-false) 
      (mapcar 
       (lambda (form) (multiple-value-list  (cps-tform form context)))
       (list cond-form if-true-form if-false-form))
      (if (not (some #'is-dirty (list cps-cond cps-true cps-false)))
	  form
	  (let ((gen-post-cond 
		 (lambda (if-form)
		   (let ((ret-sym (gensym)))
		     (values
		      (if (or (is-dirty cps-true) (is-dirty cps-false))
			  
			  (lambda (form)
			    `(scope-call 
			      (lambda ()
				(if ,if-form
				    ,(cpsListToForm cps-true)
				    ,(cpsListToForm cps-false)))
			      (lambda (,ret-sym) ,form)))
		    
			  (lambda (form)
			    `(let ((,ret-sym 
				    (if ,if-form
					,if-true-form 
					,if-false-form)))
			       ,form)))

		      ret-sym)))))
	(if (is-dirty cps-cond)
	    (multiple-value-bind (cc sym)
		(funcall gen-post-cond (second cps-cond))
	      (values
	       (lambda (form)
		 (funcall (first cps-cond) (funcall cc form)))
	       sym)) 
	    (funcall gen-post-cond cond-form))))))))
	
		
	
(setf (gethash 'let *special-forms*) #'let>cps)
(setf (gethash 'yield *special-forms*) #'yield>cps)
(setf (gethash 'progn *special-forms*) #'progn>cps)
(setf (gethash 'labels *special-forms*) #'labels>cps)
(setf (gethash 'tagbody *special-forms*) #'tagbody>cps)
(setf (gethash 'block *special-forms*) #'block>cps)
(setf (gethash 'return-from *special-forms* ) #'return-from>cps)
(setf (gethash 'lambda *special-forms*) #'lambda>cps)
(setf (gethash 'funcall *special-forms*) #'funcall>cps)
(setf (gethash 'apply *special-forms*) #'apply>cps)
(setf (gethash 'if *special-forms*) #'if>cps)

(defun test-cps2 (form)
  (cpsListToForm  (multiple-value-list (cps-tform form nil))))


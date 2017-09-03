(defpackage :test (:use cl)
	    (:export :get-tests :register :run-test :run-tests :test-this-package
		     :clear-package-tests :assert)
	    (:shadow :assert)
	    )
(in-package :test)

(defvar *all-tests* ())
(defvar *test-table* (make-hash-table))
(defstruct test
  (symbol)
  (args))

(defmacro assert (expression &optional (error-message "") &rest args)
  (if (equal error-message "")
      `(let ((result ,expression))
	 (unless result
	   (error (format nil "Assert ~a failed" ',expression))))
      `(let ((result ,expression))
	 (unless result
	   (error (format nil "Assert ~a failed: ~a" ',expression 
			  (format nil ,error-message ,@args)))))))


(defun register (function &rest arg-sets)
  (setf (gethash function *test-table*) arg-sets))

(defun get-tests (&key (package :all-packages))
  (when (and (symbolp package) (not (eq package :all-packages)))
    (setf package (find-package package)))
  (flet ((valid-package (symbol)
	   (if (eq package :all-packages)
	       t
	       (eq (symbol-package (test-symbol symbol)) package)
	       )))
    (remove-if-not #'valid-package
		   (mapcar (lambda (a-cons) (make-test :symbol (car a-cons)
						       :args (cdr a-cons)))
			   (alexandria:hash-table-alist *test-table*)))))

(defun run-test (test)
  (let ((fcn (symbol-function (test-symbol test))))
    (if (null (test-args test))
	(funcall fcn)
	(loop for test-set in (test-args test) collect
	     (if (listp test-set)
		 (apply fcn test-set)
		 (funcall fcn test-set))))))

(defun run-tests (&optional (pkg :all-packages) (debug-errors nil))
  (loop for test in (get-tests :package pkg) collect
       (list :function (test-symbol test)
	     :args (test-args test)
	     :result (if debug-errors
			 (run-test test)
			 (handler-case
			     (run-test test)
			   (error (e) (format nil "ERROR: ~a" e))
			   (:no-error (e) :no-error))))))


(defun clear-package-tests (pkg)
  (let ((tests (get-tests :package pkg)))
    (loop for test in tests do
	 (remhash (test-symbol test) *test-table*))))

(defun test-this-package ()
  (run-tests *package*))

(clear-package-tests :test)

(register
  (defun count-test-test ()
    (unless (eq 5 (length (get-tests :package :test)))
      (error "wrong length of tests"))))

(register
  (defun count-test-test2 (x)
    (error "No scratch that.. #PASS!"))
  1 2 3)
(register
 (defun multi-sum-test (a b c d)
   (unless (eq d (+ a b c))
     (error "Unexpected result ~a ~a" d (+ a b c))))
 '(1 2 3 6) 
 '(4 5 6 15))

(register 
 (defun run-test-test ()
   (run-test (make-test :symbol 'multi-sum-test 
			:args '((1 2 3 6) (4 5 6 15))))
   (handler-case
       (run-test (make-test :symbol 'multi-sum-test 
			:args '((1 2 3 7))))
     (error (e) :ok)
     (:no-error (e) (error "Should have had error")))))
(register

(let ((running nil))
  (defun test-run-all-tests ()
    (unless running
      (setf running t)
      (unwind-protect
	   (let ((all-tests (get-tests)))
	     (unless (> (length all-tests) 3)
	       (error "All tests not valid length"))
	     (eq (length (run-tests)) (length all-tests)))
	(setf running nil))))
  ))
     


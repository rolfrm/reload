(declaim (optimize (speed 3) (safety 0) (space 0) (debug 0)))
(defun-cc ccmap (func seq)
    (if seq
	(let ((value (funcall func (car seq))))
	  (cons value (ccmap func (cdr seq))))
	seq))

(defun iter2 (func seq)
  (if seq
      (cons (funcall func (car seq)) 
	    (iter2 func (cdr seq)))
	nil))

(defmacro cps-lambda (args body)
  `(lambda ,args ,(cpsListToForm  (multiple-value-list (cps-tform body nil)))))
  
(defun run-cps-to-completion (continuation)
  (if (functionp continuation)
      (run-cps-to-completion (funcall continuation))
      continuation))
(defun test-cps3 ()
  (values
   #'test-cps3 1))

(defun-cc test-cps2 ()
  (loop for x from 0 to 1000 do
       (yield x)))

(defun list-to-2d-array (lst)
  (make-array (list (length lst)) :initial-contents lst))

    
(defun run-test-cps2 (n)
  (let ((lmb (cctest2)))
    (let ((funcs (loop for x from 0 to n collect lmb)))
      (setf funcs (list-to-2d-array funcs))
      (print "..")
      (loop while (functionp (aref funcs 0)) do
	   (sleep 0.1)
	   (time
	   (loop for x from 0 to n do
		(setf (aref funcs x) (funcall (aref funcs x)))))
	   (print ".."))
      "")))

(defmacro yeild ()
  ;;Yeilds from a premptive function
  ;;Locally defined macro
  ())

(defmacro defpremp (args &rest body)
  ;;Defines a premptive function
  ;;Premptive functions can return data to other premptive functions
  ())

(defun tick (func)
  ;;iterates a premptive function
  ())

(defun example-of-use()
  "Should say 'hello from task1' and 'hello from task2' every 5 seconds"
  (progn
    (defpremp in-5 ()
      (let ((start now))
	(do while (> (- start (now)) 0) do
	  (yeild))))
    (defpremp test(text)
      (loop while t do
	   (progn
	     (in-5)
	     (print text))))
    (let ((task1 (make-premp test "hello from task1"))
	  (task2 (make-premp test "hello from task2")))
      (loop while t do
	   (progn
	     (tick task1)
	     (tick task2))))))      

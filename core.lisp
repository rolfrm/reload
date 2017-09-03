(defun read-whole-stream (stream)
  (if (not stream)
      ""
      (let ((eof nil) (out (list)))
	(loop while (not eof) do
	     (progn
	       (multiple-value-bind (str neof)
		   (read-line stream nil)
		 (progn
		   (setf eof neof)
		   (unless eof
		     (push str out))
		   ))))
	out)))

(defun read-file (path)
  (with-open-file (stream path)
    (read-whole-stream stream)))

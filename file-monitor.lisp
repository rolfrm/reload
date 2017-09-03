(defpackage :file-monitor (:use :cl)
	    (:export :file-dir-monitor 
		     :add-watched-file 
		     :monitor-file-changes))
(in-package :file-monitor)

(defun accept-all (x)
  (declare (ignore x))
  t)

(defun accept-none (x)
  (declare (ignore x))
  nil)

(defclass file-dir-monitor ()
  ((path :initarg :path)
   (filtering-function :initform #'accept-all 
		       :initarg :filtering-function)
   
   (new-file :initform #'accept-none 
	     :initarg :new-file)
   (file-changed :initform #'accept-none 
		 :initarg :file-changed)
   (file-deleted :initform #'accept-none 
		 :initarg :file-deleted)
   (watched-files :initform (make-hash-table :test 'equal))))

(defmethod get-files-in-path ((fdirmon file-dir-monitor))
  (with-slots (filtering-function path) fdirmon
    (mapcar #'truename 
	    (remove-if-not filtering-function 
			   (cl-fad:list-directory path)))))

(defmethod add-watched-file ((fdirmon file-dir-monitor) path)
  (let ((timestamp 
	 (if (probe-file path)
	     (file-write-date path)
	     nil)))
    (with-slots (watched-files) fdirmon
      (setf (gethash path watched-files) timestamp))))

(defmethod monitor-file-changes ((fdirmon file-dir-monitor))
  (with-slots( new-file file-changed file-deleted watched-files) fdirmon
    (let ((files (alexandria:hash-table-keys watched-files)))
      (loop for x in files do
	   (let ((current-value (gethash x watched-files))
		 (timestamp (file-write-date x)))
	     (if (null current-value)
		 (progn
		   (funcall new-file x)
		   (funcall file-changed x))
		 (unless (equal current-value timestamp)
		   (funcall file-changed x)))
	     (setf (gethash x watched-files) timestamp)))
      (let ((missing-files 
	     (set-difference 
	      (alexandria:hash-table-keys watched-files) files :test 'equal)))
	(loop for x in missing-files do
	     (print (format nil "~a deleted" x))	     
	     (remhash x watched-files))))
    (force-output)))

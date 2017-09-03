(defpackage :queue (:use :cl)
	    (:export :queue :make-queue
		    :queue-first :queue-last
		    :enq :deq :peek
		    :queue-length
		    :queued?))
(in-package :queue)

(defstruct queue
  (first nil)
  (last nil))

(defun enq (queue obj)
  (let ((fcons (cons obj nil)))
    (if (queue-first queue)
	(setf (cdr (queue-last queue)) fcons)
	(setf (queue-first queue) fcons))
    (setf (queue-last queue) fcons)
    queue))

(defun deq (queue)
  (let ((out (car (queue-first queue))))
    (if (eq (queue-first queue) (queue-last queue))
	(progn
	  (setf (queue-first queue) nil)
	  (setf (queue-last queue) nil))
	(setf (queue-first queue) (cdr (queue-first queue))))
    out))

(defun peek(queue)
  (first (queue-first queue)))

(defun queue-length (queue)
  (length (queue-first queue)))

(defun queued?(queue)
  (not (null (queue-first queue))))

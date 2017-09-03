(defpackage :event
  (:use :cl)
  (:export :event :add-listener :set-listeners 
	   :remove-listeners :invoke :clear-listeners))
(in-package :event)

(defun ffold (subs &rest args)
  (mapcar (lambda (f) (apply f args)) subs))

(defclass event () 
  ((subscribers :initform (list))))

(defmethod add-listener ((evt event) delegate)
  (with-slots(subscribers) evt
    (push delegate subscribers)))

(defmethod set-listeners ((evt event) delegates)
  (with-slots (subscribers) evt
    (setf subscribers delegates)))

(defmethod remove-listener ((evt event) delegate)
  (with-slots (subscribers) evt
    (setf subscribers (remove delegate subscribers))))

(defmethod invoke ((evt event) &rest args)
  (with-slots (subscribers) evt
    (apply #'ffold subscribers args)))

(defmethod clear-listeners ((evt event))
  (with-slots (subscribers) evt
    (setf subscribers (list))))


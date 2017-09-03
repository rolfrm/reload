(re.load "lru-table.lisp")
(defpackage :item-manager
  (:use :cl)
  (:export :make-item-manager :get-item :clear-items :item-count :get-items)
  )

(in-package :item-manager)
(defclass item-manager ()
  ((src2item :initform (make-hash-table :test 'equal))
   (max-items :initarg :max-items :initform 10000000)
   (load-fcn :initform (lambda (x) ) :initarg :load-fcn)
   (delete :initform (lambda (outp) ) :initarg :delete)
   (hash :initform (lambda (x) x) :initarg :hash)
   (lru :initform (make-instance 'lru:lru-table))
   ))

(defun get-items (item-manager)
  (with-slots(src2item) item-manager
    (alexandria:hash-table-values src2item)))

(defun make-item-manager (load-function delete-function max-number-of-items &optional (hash (lambda (x) x)))
  (make-instance 'item-manager :load-fcn load-function
		 :delete delete-function
		 :hash hash
		 :max-items max-number-of-items))
(defun remove-hash (item-manager hash)
  (with-slots (lru delete src2item) item-manager
    (multiple-value-bind (item hash-ok)
	(gethash hash src2item)
      (unless hash-ok
	(error "Unknown hash"))
      (remhash hash src2item)
      (lru:remove lru hash)
      (funcall delete item))))
    
(defun delete-least-recently-used (item-manager)
  (with-slots (lru) item-manager
    (let ((hash (lru:get-least-recently-used lru)))
      (remove-hash item-manager hash))))

(defun make-room( item-manager)
  (with-slots(lru delete max-items src2item) item-manager
    (let ((limit (- max-items 1)))
      (loop while (> (lru:size lru) limit) do
	   (delete-least-recently-used item-manager)))))

(defmethod get-item((item-manager item-manager) prototype)
  (with-slots(lru hash src2item load-fcn max-items) item-manager
    (let ((hash-value (funcall hash prototype)))
      (multiple-value-bind (hashv ishash)
	  (gethash hash-value src2item)
	(if ishash
	    hashv
	    (progn
	      (make-room item-manager)
	      (let ((new-value (funcall load-fcn prototype)))
		(setf (gethash hash-value src2item) new-value)
		(lru:insert lru hash-value)
		new-value))
	    )))))

(defmethod clear-items (object))

(defmethod clear-items ((item-manager item-manager))
  (with-slots (lru) item-manager
    (loop while (> (lru:size lru) 0) do
	 (delete-least-recently-used item-manager))))

(defmethod item-count ((item-manager item-manager))
  (with-slots (lru) item-manager
    (lru:size lru)))

(defpackage :item-manager-test (:use :cl :item-manager))
(in-package :item-manager-test)
(test:register
 (defun item-manager-test ()
   (let* ((item-table ())
	 (manager (make-item-manager (lambda(proto) (+ 10 proto))
				     (lambda (item) (push item item-table))
				     5 ))
	  )
     (let ((collected (loop for x from 0 below 10 collect (get-item manager x))))
       (unless (and (find 15 collected) (eq (length item-table) 5))
	 (error (format nil "Unexpected values ~a ~a" collected item-table)))
       (clear-items manager)
       (unless (eq (item-count manager) 0)
	 (error "Clear didnt work"))
       (print item-table)
       (unless (eq (length item-table) 10)
	 (error "objects not deleted"))
       )))
 )

(defpackage :lru-table (:use :cl) (:export :lru-table :insert :remove
					   :get-least-recently-used
					   :size	   )
	    (:shadow :remove)
	    (:nicknames :lru)
)
(in-package :lru-table)
(defclass lru-table ()
  ((elements :initform (make-hash-table :test 'equal)) ;element -> cons table
   (conses :initform (cons nil nil))))

(defmethod insert ((table lru-table) element)
  (with-slots (elements conses) table
    (when (gethash element elements)
      (remove table element))
    (setf (gethash element elements) conses) ;;on top
    (let* ((next-form (cdr conses))
	  (new-form (cons element next-form)))
      (setf (cdr conses) new-form)
      (when next-form
	(setf (gethash (car next-form) elements) new-form))
      )))

(defmethod get-least-recently-used ((table lru-table))
  (with-slots (conses) table
    (car (last conses))))

(defmethod size ((table lru-table))
  (with-slots (elements) table
    (hash-table-count elements)))

(defmethod remove ((table lru-table) element)
  (with-slots (elements conses) table
    (multiple-value-bind (hash-value hash-exists)
	(gethash element elements)
      (when hash-exists
	(setf (cdr hash-value) (cdr (cdr hash-value)))
	(let ((sub-elem (cadr hash-value)))
	  (when sub-elem
	    (setf (gethash sub-elem elements) hash-value)))
	(remhash element elements)))))
(defpackage :lru-test (:use :cl))
(in-package :lru-test)

(defun insert-remove()
  (let ((table (make-instance 'lru-table:lru-table)))
     
     (loop for x from 0 below 10 do
	  (lru-table:insert table x))
     (loop for x from 5 downto 0 do
	  (lru-table:insert table  x))
     
     (let ((lru (lru-table:get-least-recently-used table)))
       (unless (eq lru 6)
	 (error "Error in lru table"))
       (lru-table:remove table lru))
     (unless (eq (lru-table:get-least-recently-used table) 7)
       (error "new value error"))
     (unless (eq (lru-table:size table) 9)
       (error (format nil "table size wrong:~a should be 9 " (lru-table:size table)))
     )))
(defun insert2()
  (let ((table (make-instance 'lru-table:lru-table)))
    (loop for x from 0 below 5 do
	   (lru-table:insert table x))
    (loop for x from 0 below 5 do
	 (lru-table:insert table x))
    (unless (eq (lru-table:size table) 5)
      (error (format nil "Unexpected table size ~a" (lru-table:size table))))))

(defun symbol-test ()
  (let ((table (make-instance 'lru-table:lru-table)))
    (lru:insert table 'a)
    (lru:insert table 'b)
    (lru:insert table 'c)
    (lru:insert table 'a)
    (unless (eq (lru:get-least-recently-used table) 'b)
      (error ("Unexpected element")))))
 

(test:register 'insert-remove)
(test:register 'insert2)
(test:register 'symbol-test)

(defun string-test ()
  (let ((table (make-instance 'lru-table:lru-table)))
    (lru:insert table "a")
    (lru:insert table "b")
    (lru:insert table "a")
    (unless (equal (lru:get-least-recently-used table) "b")
      (error "Undexpected string thingy"))))
(test:register 'string-test)


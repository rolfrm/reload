(defclass test-class ()
  ((name :initform 5 :writer set-name)))

(defclass test-class-4 ()
  ((name :initform 5 :writer test-write )))

(defmethod test-class-name (new-name (self test-class))
  (with-slots (name) self
    (setf name (print new-name))))


(defmethod get-test-class-name ((self test-class))
  (with-slots (name) self
     (print name)))

(defclass test-class-2 (test-class)
  ((name :allocation :class :initform 6)))

(defclass test-class-3 (test-class)
  ((name :initform 6 :allocation :instance)))

(defclass revised ()
  ((revision :initform 0)))

(defclass rev2 (revised)
  ((inner-value :initform nil)))


(defmacro def-revised-value-setter (class slot newfun-name)
  `(defmethod ,newfun-name ((self ,class) value)
    (with-slots (slot revision) self
      (unless (equal value (slot-value self ',slot))
	(progn
	  (incf (slot-value self 'revision))
	  (setf (slot-value self ',slot) value))))))

(defun combine-symbols (&rest args)
  (intern (apply #'concatenate 'string (mapcar #'symbol-name args)))) 

(defmacro combine-two-symbols (sym-a sym-b)
  `(combine-symbols ',sym-a ',sym-b))

(defmacro def-revised-value-setter2 (class slot)
  (let ((symname (combine-symbols 'set- slot)))
    
     `(defmethod ,symname ((self ,class) value)
       (with-slots (slot revision) self
	 (unless (equal value (slot-value self ',slot))
	   (progn
	     (incf (slot-value self 'revision))
	     (setf (slot-value self ',slot) value)))))))
(print (macroexpand-1 '(def-revised-value-setter2 rev2 inner-value)))


(def-revised-value-setter rev2 inner-value set-inner-value)
;(print (macroexpand `(def-revised-value-setter rev2 inner-value)))

;;Macro using macro
(defmacro sym-fun (a b)
  (eval `(let ((s1 (combine-two-symbols ,a ,a)) (s2 (combine-two-symbols ,b ,b)))
    `(combine-two-symbols ,s1 ,s2))))


(defun symfun-ex ()
  (combine-symbols (combine-symbols 'a 'b) (combine-symbols 'c 'd)))

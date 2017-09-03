(defpackage cl-package-system (:use cl))
(in-package cl-package-system)

(defmacro export-syms-package (package-name syms)
  `(progn 
    (defpackage ,package-name (:use cl cl-package-system))
    (in-package ,package-name)
    (export-syms ',syms)))

(defun export-syms (syms)
  (loop for sym in syms do  (export sym)))

(export-syms '(export-syms export-syms-package))

(export-syms-package cl-math (+ - * / sin cos tan expt))
(export-syms-package cl-core 
		     (defun defmacro defpackage in-package defvar defmethod defclass defgeneric defparameter
			    progn if let cond case when unless with-slots setf aref lambda 
			    loop print 'nil t error intern export eq eql equal null functionp
			    integer single-float double-float string fixnum type-of
			    defstruct &rest &key
			    ))
(export-syms-package cl-sym (symbol-name))
(export-syms-package cl-seq (list list* push pop mapcar make-array copy-list map concatenate first second nth vector))



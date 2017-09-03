(defstruct cps-context
  (dirty-functions (hash-table-keys *cps-functions*))
  (block-scope '())
  )


(defmacro add-to-struct-list-fn (lst-name)
  (let ((symname (concat-symbols 'add-to- lst-name))
	(getname  (concat-symbols 'get- lst-name))
	(accessor (concat-symbols 'cps-context- lst-name)))
    `(progn
       (defun ,symname (cps-context func)
	 (progn
	   (when (null cps-context)
	     (setf cps-context (print (make-cps-context))))
	   (setf cps-context (copy-structure cps-context))
	   (push func (,accessor cps-context))
	   cps-context))
       (defun ,getname (cps-context)
	 (if cps-context
	     (,accessor cps-context)
	     (,accessor (make-cps-context)))))))

(add-to-struct-list-fn dirty-functions)
(add-to-struct-list-fn block-scope)


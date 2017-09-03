(re.load "with-package-macro/with-package.lisp")  

(defpackage :utils (:use :cl :with-package))
(in-package :utils)

(defun ex> (&rest forms)
  "exports the symbols in the list forms"
    (loop for sym in forms do (when (symbolp sym) (export sym))))

(export 'ex>)

(ex>
(defun from-item (item lst)
  (if (equal (first lst) item)
       lst
      (from-item item (rest lst))))

(defun rest-from-item (item lst)
  (rest (from-item item lst)))

(defun random-range(from to)
  (let ((range (- to from)))
    (+ (random range) from)))

(defmacro post-incf (var &optional (amount 1))
  "Increment then return the old value"
  `(let ((current-val ,var))
     (incf ,var ,amount)
     current-val))

(defmacro post-set (var value)
  `(let ((current-val ,var))
     (setf ,var ,value)
     current-val))

(defun cps-transform (form)
  (progn
    (print form)
    (loop for sub in form do
	 
	 (if (listp sub)
	   (cps-transform sub)
	   (print sub)))))

(defmacro cps (form)
  (print (cps-transform form)))

(defun add-last (lst elem)
  (append lst (list elem)))

(defmacro push-front (elem lst)
  `(setf ,lst (add-last ,lst ,elem)))

(defun take-while-split(predicate sequence)
  (let ((first nil) (second sequence))
    (loop while (and second (funcall predicate (first second))) do
	 (setf first (add-last first (first second)))
	 (setf second (rest second)))
    (values first second)))

(defun printf (format-string &rest rest)
  (let ((first-element (first rest)))
    (print (apply #'format nil format-string rest))
    first-element))

(defmacro print-values (subform)
  `(let ((lst (multiple-value-list ,subform)))
    (printf "values: ~a" lst)
    (apply #'values lst)))

(defun concat-symbols(sym1 sym2 &optional (sep-string "") (package nil))
  (let ((symname (concatenate 'string (symbol-name sym1) sep-string (symbol-name sym2))))
    (if package
	(intern symname package)
	(intern symname))))

(defun string-join (delimiter &rest strings)
  (if strings
      (if (rest strings)
	  (concatenate 'string (first strings) delimiter (apply #'string-join delimiter (rest strings)))
	  (first strings))
      ""))


(defmacro export-all-symbols()
  (let ((pack *package*))
    (do-all-symbols (sym pack) (when (eql (symbol-package sym) pack) (export  sym)))))

(defun in-range (first-pred second-pred first-value second-value test-value)
  (and (funcall first-pred first-value test-value)
       (funcall second-pred second-value test-value)))

(defun take(seq n)
  (loop for x from 0 below n for y in seq collect y))

(defun symbol-to-keyword (sym)
  (alexandria:make-keyword (symbol-name sym)))

(defmacro unfold-slot (object &rest slots)
  (if slots
      `(unfold-slot (slot-value ,object ',(first slots)) ,@(rest slots) )
      object))

(defun on-slot (object &rest slots)
  (if slots
      (on-slot (slot-value object (first slots)))
      object))

(defun sorted-insert (lst obj &key (key (lambda (x) x)) 
				(test #'<))
  (let ((obj-value (funcall key obj)))
    (labels ((sort-iterate (lst)
	       (let ((first-obj (first lst)))
		 (if lst
		     (let ((key (funcall key (first lst))))
		       (if (funcall test obj-value key)
			   (cons obj lst)
			   (cons first-obj (sort-iterate (rest lst)))))
		       (cons obj nil)))
	       ))
		       
      (sort-iterate lst))))

(defun ordered-insert (num lst)
  (sorted-insert lst num :test #'>))

(defun i-sort (lst)
  (if (null lst) 
      nil
      (sorted-insert (i-sort (rest lst)) 
		     (first lst) )))


(defun random-element(lst)
  (let ((list-length (length lst)))
    (nth (random list-length) lst)))

(defun remove-first (item seq)
  (when seq
    (if (eq (car seq) item)
	(cdr seq)
	(cons (car seq) (remove-first item (cdr seq))))))
      
(defun export-class (class-sym)
  (loop for sym in (reflect:class-slot-list (find-class class-sym)) do
       (export sym)))

(defun get-time-of-day()
  (sb-ext:get-time-of-day))

(defun accurate-time ()
  (multiple-value-bind (seconds micro-seconds)
      (get-time-of-day)
    (+ seconds (/ micro-seconds 1000000))))

(defmacro let-list (names lst &rest body)
  (let ((list-sym (gensym)))
  `(let ((,list-sym ,lst))
     (let* ,(mapcar (lambda (name) (list name `(pop ,list-sym))) names)
	   ,@body))))

(defun add-assoc (key object alist)
  (cons (cons key object) alist))

(defun add-assocs (alist &rest key-values)
  (if key-values
      (let ((new-assoc (add-assoc (pop key-values) (pop key-values) alist)))
	(apply #'add-assocs new-assoc key-values))
      alist))

 (defun get-assoc (key alist)
   (cdr (assoc key alist :test 'equal)))

;(defun get-assocs (alist &rest keys)
;  (mapcar (alexandria:rcurry #'get-assoc alist) keys))

;; (defun get-assocs2 (alist &rest keys)
;;   (when (and keys alist)
;;     (let ((cons (car alist)))
;;       (let ((found-key (find (car cons) keys)))
;; 	(if found-key
;; 	    (cons (cdr cons) (apply #'get-assocs2 (cdr alist) (remove-first (car cons) keys)))
;; 	    (apply #'get-assocs2 (cdr alist) keys))))))

;; (defun get-assocs3 (alist &rest keys)
;;   (when (and alist keys)
;;     (print keys)
;;     (sleep 0.4)
;;     (let ((found-key (find (first keys) alist :key #'car)))
;;       (if found-key
;; 	  (cons found-key (apply #'get-assocs3 (remove-first found-key alist) (cdr keys)))
;; 	  (apply #'get-assocs3 alist (cdr keys))))))

(defun get-assocs (alist &rest keys)
  (when (and alist keys)
    (let ((found-key (find (first keys) alist :key #'car :test 'equal)))
      (if found-key
	  (cons(cdr found-key) (apply #'get-assocs (remove-first found-key alist) (cdr keys)))
	  (cons nil (apply #'get-assocs alist (cdr keys)))))))

(defmacro assoc-bind (symbols alist &rest body)
  (let ((search-symbols nil)
	(search-targets nil))
    (loop for sym in symbols do
	 (if (listp sym)
	     (let ((symbol (car sym))
		   (target (car (cdr sym))))
	       (push  symbol search-symbols)
	       (push (list 'quote target) search-targets))
	     (progn
	       (push sym search-symbols)
	       (push (list 'quote sym) search-targets))))
    `(let-list ,(reverse search-symbols)
	       (get-assocs ,alist ,@(reverse search-targets))
       ,@body)))

(defun find-assocs(alist pred &rest assoc-names)
  ())
       
(defun test-find-assocs()
  (let ((assoc-output (find-assocs '((:a . 1) (:a . 5) (:a . 7)) (lambda (assoc-name assoc-value) (> assoc-value 4))  :a :a)))
    (test:assert (equal assoc-output 5 7))))
;; * Assoc test *;;

(defun test-assocs ()
  (let ((assoc-output (get-assocs '((:a . 1) (:b . 2) (:c . 2)) :a :d :c)))
    (test:assert (eq (length assoc-output) 3))))
(test:register 'test-assocs)

(defun test-assoc-bind ()
  (let ((test-data '((:shader . 1) (:window . 2) (:renderer . 3))))
    (assoc-bind 
     ((shader :shader) (something-invalid :something-invalid) (window :window)) test-data
     (test:assert (and (eq shader 1) (eq window 2) (null something-invalid)) "shader(1) ~a, window(2) ~a" shader window)
     )))
(test:register 'test-assoc-bind)
		

(defun float-equal (a b &optional (lim 0.00000001))
  (let ((r (- a b)))
    (< (abs r) lim)))

(defmacro with-hashes (hash-bindings hash-table &rest body)
  (let* ((hashsym (gensym))
	(symbnd (mapcar 
		 (lambda (sym) 
		   (let ((srcsym (if (listp sym) (first sym) sym))
			 (dstsym (if (listp sym) (second sym) sym)))
		     (list dstsym (list 'gethash srcsym hashsym))))
		 hash-bindings)))
    `(let ((,hashsym ,hash-table))
       (symbol-macrolet ,symbnd
	 ,@body))))

)



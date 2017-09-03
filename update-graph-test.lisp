(declaim (optimize (speed 0) (debug 3) (safety 3)))
(defun levenshtein-distance (str1 str2)
  "Calculates the Levenshtein distance between str1 and str2, returns an editing distance (int)."
  (let ((n (length str1))
        (m (length str2)))
    ;; Check trivial cases
    (cond ((= 0 n) (return-from levenshtein-distance m))
          ((= 0 m) (return-from levenshtein-distance n)))
    (let ((col (make-array (1+ m) :element-type 'integer))
          (prev-col (make-array (1+ m) :element-type 'integer)))
      ;; We need to store only two columns---the current one that
      ;; is being built and the previous one
      (dotimes (i (1+ m))
        (setf (svref prev-col i) i))
      ;; Loop across all chars of each string
      (dotimes (i n)
        (setf (svref col 0) (1+ i))
        (dotimes (j m)
          (setf (svref col (1+ j))
                (min (1+ (svref col j))
                     (1+ (svref prev-col (1+ j)))
                     (+ (svref prev-col j)
                        (if (char-equal (schar str1 i) (schar str2 j)) 0 1)))))
        (rotatef col prev-col))
      (svref prev-col m))))

(defun weighted-levenshtein-distance (str1 str2)
  (let ((ld-value (levenshtein-distance str1 str2)))
    (if (eq ld-value 0)
	1.0
	(let ((l2 (length str2)))
	  (if (equal l2 0)
	      0.0
	      (- 1 (/ ld-value l2))))))) 

(defclass object-matrix ()
  ((table :initform (make-hash-table :test 'equal))))

(defmethod insert-value ((self object-matrix) row col value)
  (with-slots (table) self
    (setf (gethash (list row col) table) value)))

(defmethod get-value ((self object-matrix) row col)
  (with-slots (table) self
    (gethash (list row col) table)))

(defmethod remove-dim ((self object-matrix) dim nr)
  (with-slots (table) self
    (let ((to-remove 
	   (remove-if-not 
	    (lambda (x) (equal (nth dim x) nr)) 
	    )))
      (loop for rem in to-remove do
	   (remhash rem table)))))

(defmethod remove-column ((self object-matrix) col)
  (remove-dim self 1 col))

(defmethod remove-row ((self object-matrix) row)
  (remove-dim self 0 row))

(defmethod map-node (a b)
  (if (equal a b)
      1.0
      0.0))

(defmethod map-node ((a string) (b string))
    (let ((streq (weighted-levenshtein-distance a b)))
      streq))

(defmethod map-node ((a list) (b list))
  (update-graph a b))

(defun printf (formatting &rest args)
  (progn
    (print (apply #'format nil formatting args))
    (if args
	(first args)
	nil)))

(defun permute-zip (&rest args)
    (if (rest args)
	(let ((outlist (list)))
	  (loop for x in (first args) do
	       (loop for y in (apply #'permute-zip (rest args)) do
		    
		    (push (list* x y '()) outlist)))
	  outlist)
	(first args)))

(defun +gen-equality-matrix (g1 g2)
  (let ((equalities '()))
    (loop for old-subgraph in (unroll-graph g1) do
	 (loop for new-subgraph in (unroll-graph g2) do
	      (let ((equality (map-node old-subgraph new-subgraph)))
		(when (> equality 0)
		  (push (list old-subgraph new-subgraph equality)  equalities)))))
    equalities))

(defmacro mcurry (function &rest args)
  `(curry #',function ,@args))


;;;Splít list into group sized n
(defun group (source n)
       (if (zerop n) (error "zero length"))
       (labels ((rec (source acc)
                     (let ((rest (nthcdr n source)))
                        (if (consp rest)
                             (rec rest (cons (subseq source 0 n) acc))
                             (nreverse (cons source acc))))))
         (if source (rec source nil) nil)))

(defun bigger-than (x)
  (rcurry #'> x))

(defun +map-filter-graph (g1 g2)
  (flet ((get-equality (pair)
	   (list (first pair) (second pair) 
		 (map-node (first pair) (second pair)))))
    (let* ((unrolled-g1 (unroll-graph4 g1))
	   (unrolled-g2 (unroll-graph4 g2))
	   (equalities
	    (remove-if-not 
	     (bigger-than 0)
	     (mapcar #'get-equality (permute-zip unrolled-g1 unrolled-g2))
	     :key #'third)))
      (values unrolled-g1 unrolled-g2 equalities))))

(defun +filter-equalities (equalities)
  (let ((sorted-equalities (sort equalities #'> :key #'third)))
    (loop while sorted-equalities collect
	 (let ((max-entry (first sorted-equalities)))
	   (setf sorted-equalities 
		 (remove-if 
		  (lambda (x) (or 
			       (eq (first x) (first max-entry))
			       (eq (second x) (second max-entry))))
			  sorted-equalities))
	   max-entry
	   ))))

(defun f2 (x selected new-entries)
  (let ((selected-found (find x selected :key #'second)))
    (if selected-found
	(if (eq 1.0 (third selected-found))
	    (first selected-found)
	    (mapcar (lambda (x) 
		      (if (listp x)
			  (print (f2 x selected new-entries))
			  x)) 
		    (second selected-found)))
	(let ((new-found (find x new-entries)))
	  (if new-found
	      (if (listp new-found)
		  (mapcar (rcurry #'f2 selected new-entries) new-found)
		  new-found)
	      x))))) ;;Completely new element

(defun +calc- (selected new-entries unrolled-g1)
  (let* ((n-total-entries (length (append selected new-entries)))
	 (remove-entries (set-difference unrolled-g1 (mapcar #'first selected))))
    (if (eq n-total-entries 0)
	0.0
	(/ (reduce #'+ (mapcar #'third selected)) 
	   (+ (length remove-entries) n-total-entries))
	)))

(defun update-graph (g1 g2)
  (multiple-value-bind (unrolled-g1 unrolled-g2 equalities)
      (+map-filter-graph g1 g2)
    (let ((selected (+filter-equalities equalities)))
      (let ((new-entries (set-difference unrolled-g2 (mapcar #'second selected))))
	(values 
	 (+calc- selected new-entries unrolled-g1) 
	 (mapcar (rcurry #'f2 selected new-entries) g2))))))


(defun not-sized-list(lst)
  (if (listp lst)
      (null lst)
      t))
(defun contains-sub-list (lst)
  (some #'listp lst))

(defun unroll-graph (g)
  (let* ((sublists (remove-if-not (lambda (x) (and (size-list x) (contains-sub-list  x))) g))
	 (kept-lists (set-difference g sublists)))
					
    (apply #'append kept-lists (remove-if-not #'size-list (mapcar #'unroll-graph sublists)))))

(defun unroll-graph2 (g)
  (let* ((sublists (remove-if-not (lambda (x) (and (size-list x) (contains-sub-list  x))) g)))
    (if sublists
	(apply #'append (printf "g: ~a" g) (printf "mapc: ~a" (mapcar #'unroll-graph2 sublists)))
	g)))

(defun unroll-graph3 (g)
  (let ((outlist g))
    (loop for x in (remove-if-not #'listp g) collect
		(setf outlist (append outlist (remove-if-not #'listp (unroll-graph3 x)))))
    outlist))

(defun unroll-graph4 (g)
  (append g
	  (remove-if #'not-sized-list
	   (apply #'append '() 
		  (mapcar #'unroll-graph4 
			  (remove-if-not #'listp g))))))

(defun test()
  (let ((lst1 '((a 5) (b 10) (test2 :b (lambda (x) (print (+ 1 x)))) (c 20)))
	(lst2 '((a 5) (b 15) (test4 (test3 (test2 :b (lambda (x) (print (+ 1 x))))))(c 30))))

    (multiple-value-bind (x y)
      	(map-node lst1 lst2)
      (progn
	(mapcar (lambda (x y) (print (eq (print x) (print y)))) lst2 y))
      y)))


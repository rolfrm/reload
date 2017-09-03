(defmethod serialize (any-object)
  any-object)

(defmethod serialize ((lst list))
  (mapcar #'serialize lst))

(defmethod serialize ((fnc function))
  (multiple-value-bind (lmbd closures name)
      (function-lambda-expression fnc)
    (if lmbd
	lmbd
	(list 'func name))))

(defmethod serialize ((htb hash-table))
  (list 'hashtable (hash-table-plist htb) (hash-table-test htb)))

(defmethod deserialize (anydata)
  anydata)

(defmethod deserialize ((data list))
  (if (null data)
      '()
      (let ((fst (first data)))
	(case fst
	  (func (eval (list 'function (second data))))
	  (hashtable (plist-hash-table (second data) :test (third data)))
	  (lambda (eval data))
	  (otherwise (mapcar #'deserialize data))))))
(defmethod serialize ((peer tcp-peer))
  (with-slots (ip port) peer
    (list ip port))) 

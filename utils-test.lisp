(defpackage :utils-test
  (:use :cl))
(in-package :utils-test)
(test:clear-package-tests :utils-test)
(defun assoc1()
  (assert (eq 1 (utils:get-assoc :test1 (utils:add-assoc :test1 1 ()))))
  (let ((assoc-list (utils:get-assocs (print '((a . 1) (:b . 2) (|c| . 3)))
			'a :b '|c|)))
    (assert (eq (first assoc-list) 1))
    (destructuring-bind (a b c) assoc-list
      (assert (and (eq a 1) (eq b 2) (eq c 3))))))
(test:register 'assoc1)

(defun get-test-list()
  '((:a . 1) (b . 2) (c . a) (b . 1.5)))

(defun assoc3()
  (let ((tstlst (get-test-list)))
    (let ((ret (utils:get-assocs tstlst :a 'b 'c 'b)))
      (assert (eq (length ret) 4))
      (assert (eq (first ret) 1))
      (assert (eq (nth 3 ret) 1.5))
      )))

(test:register 'assoc3)

(defun assoc2()
  (let ((symlst (get-test-list)))
    (utils:assoc-bind 
     ((a :a) b (d c) (b2 b) e) symlst
     (assert (eq a 1))
     (assert (eq b 2))
     (assert (eq d 'a))
     (assert (eq b2 1.5))
     (assert (eq e nil))
     )))
(test:register 'assoc2)



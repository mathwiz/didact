;; Require
(load "continuation-passing.lisp")
(load "nondeterminism.lisp")


(=defun descent (n1 n2)
        (cond ((eq n1 n2) (=values (list n2)))
              ((kids n1) (choose-bind n (kids n1)
                                      (=bind (p) (descent n n2)
                                             (=values (cons n1 p)))))
              (t (fail))))


;; try it
(defun kids (n)
  (case n
    (a '(b c))
    (b '(d e))
    (c '(d f))
    (f '(g))
))


(print (descent 'a 'g))
(print (fail))
(print (descent 'a 'd))
(print (fail))
(print (fail))
(print (descent 'a 'h))

(print 'nondeterminism-tests2)

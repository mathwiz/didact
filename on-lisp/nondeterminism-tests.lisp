;; Require
(load "continuation-passing.lisp")
(load "nondeterminism.lisp")


(=defun two-numbers ()
        (choose-bind n1 '(0 1 2 3 4 5)
                     (choose-bind n2 '(0 1 2 3 4 5)
                                  (=values n1 n2))))



(=defun parlor-trick (sum)
        (=bind (n1 n2) (two-numbers)
               (if (= (+ n1 n2) sum)
                   `(the sum of ,n1 ,n2)
                   (fail))))


;; try it
(print (parlor-trick 7))

(print 'nondeterminism-tests)

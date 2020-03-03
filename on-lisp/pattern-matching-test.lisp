;; Requires
(load "pattern-matching.lisp")


;; examples
(print (match '(p a b c a) '(p ?x ?y c ?x)))

(let ((m (match '(p ?x b ?y a) '(p ?y b c a))))
  (print m))

(let ((m (match '(a b c) '(a a a))))
  (print m))

(let ((m (match '(a ?x b) '(_ 1 _))))
  (print m))



(print 'pattern-matching)

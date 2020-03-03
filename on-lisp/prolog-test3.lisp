;; Require
(load "prolog-interpreter3.lisp")


(<- (gaunt raoul))
(<- (smells-of raoul turpentine))
(<- (painter rubens))

(<- (painter ?x) (hungry ?x) (smells-of ?x turpentine))

(<- (hungry ?x) (or (gaunt ?x) (eats-ravenously ?x)))


(with-inference (painter ?x)
  (print ?x))


(<- (eats ?x ?f) (glutton ?x))

(<- (glutton hubert))


(with-inference (eats ?x spinach))
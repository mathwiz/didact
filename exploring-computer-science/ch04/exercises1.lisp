(setf (symbol-function 'a) #'sin)
(atom a)

(apply #'sin pi) ; error
(apply #'* 1 2 3) ; error
(eval '(* 1 2 3))
(eval '(sin pi))
(eval '(a pi))


(defmacro mysetq (a e)
  (list 'set (list 'quote a) e))

(mysetq x 5)
x

(macroexpand '(mysetq x (+ 1 2)))

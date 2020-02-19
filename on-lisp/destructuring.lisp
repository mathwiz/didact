;; Built-in descructuring-bind
(destructuring-bind (x y z) '(1 2 3)
  (progn
    (print x)
    (print y)
    (print z)))

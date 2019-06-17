(load "family-tree.lisp")

(print family)
(print (father 'suzanne))
(print (mother 'bruce))
(print (parents 'frederick))
(print (children 'arthur))
(print (siblings 'bruce))
(print (siblings 'zelda))
(print (mapunion #'rest '((1 a b c) (2 e c j) (3 f a b c d))))


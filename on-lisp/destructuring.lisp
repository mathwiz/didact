;; Built-in descructuring-bind
(destructuring-bind (x y z) '(1 2 3)
  (progn
    (print x)
    (print y)
    (print z)))


;; Same concept but works with any kind of sequence
(defmacro dbind (pat seq &body body)
  (let ((gseq (gensym)))
    `(let ((,gseq ,seq))
       ,(dbind-ex (destruc pat gseq #'atom) body))))


(dbind (a (b c) d) '(1 #(2 3) 4)
       (progn
         (print a)
         (print b)
         (print c)
         (print d)))


(dbind (a (b . c) &rest d) '(1 "fribble" 2 3 4)
       (progn
         (print a)
         (print b)
         (print c)
         (print d)))



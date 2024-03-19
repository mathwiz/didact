;; ************************************************
(print '(1.1 What does Lisp look like?))

(cons 'fruit
      (cdr (subst 'banana
                  'arrow
                  '(time flies like an arrow))))

(car (cons 'a 'b))
(cdr (cons 'a 'b))

(cons 'a 'nil)
(cons '4 nil)
(cons 'a (cons 'b nil))

(cdr '(a b c))

(list 'a)
(list 'b nil)
(list)

(print (quote ('b nil)))
(print (quote (cons 'a 'b)))


;; ************************************************
(print '(1.2 Conses and Atoms))  

(setq a '(a b))
(setq b 'x)
(set b 'y)

(print b)
(print x)


;; ************************************************
(print '(1.3 Lists))

;; E1.3.1
(list 'big 'cat 'sat)
(cons 'the (list 'big 'cat 'sat))
(list 'all (list 'good 'people) 'should (list 'go 'ahead))

;; 1.3.2
(car (cdr '(a b c d)))
(car (cdr (car '((a b) c d))))
(cdr (car (cdr '(a (b c) d))))


;; ************************************************
(print '(1.4 The Lisp Evaluation Rule))


;; ************************************************
(print '(1.5 Quote- The Evaluation Blocker))

;; compare to using setf
(setq a '(u v w))
(set (car (cdr a)) 'b)
(print (cons v a))


(setq a '(u v w))
'(setq a '(x y z))
(print a)


(setq a 'a)
(setq b 'a)
(print (list a b 'b))


(print (list (list 'a 'b) '(list 'a 'b)))


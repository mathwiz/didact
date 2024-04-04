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

(+ (car (cdr (list 2 3))) 4)


;; ************************************************
(print '(1.5 Quote- The Evaluation Blocker))

(quote a)
(quote (x y))
(cons 3 (quote (x y)))
(cons 3 (quote (plus 5 7)))
(quote (a b (quote c) d))
'(a b 'c d)
''a
(quote (quote a))
'a
(+ 3 4)
(+ '3 '4)
(set 'a '(x y))
a
(car a)
(set 'b '3)
(set 'y 'a)
y
(set 'z a)
z
(setq a '(a b))
(setq b 'x)
(set b 'y)
b
x

;; compare to using setf
(setq a '(u v w))
(set (car (cdr a)) 'b)
(print (cons v a))
(print v)
(print a)


(setq a '(u v w))
'(setq a '(x y z))
(print a)


(setq a 'a)
(setq b 'a)
(print (list a b 'b))


(print (list (list 'a 'b) '(list 'a 'b)))


;; ************************************************
(print '(1.6 Eval- The Evaluator Itself))

(setq a (list '+ 5 6))
a
(cons a '(is the answer))
(cons (eval a) '(is the answer))


;; ************************************************
(print '(1.7 User-Defined Functions))

(defun square (x) (* x x))
(square 65)

(defun my-third (list) (car (cdr (cdr list))))
(my-third '(1 2 3 4 5))
(my-third (my-third '(a (b c) (d e f) (g h i j))))

(defun cylinder-volume (length radius)
  (* length (* 3.14159265 (square radius))))
(cylinder-volume 2.5 2.0)

(defun double (x) (* 2 x))
(double 2.358)

(defun times-square (x y) (* x y y))
(times-square 4 3)

(defun times-cube (x y) (* x y y y))
(times-cube 3 2)


;; ************************************************
(print '(1.8 Variables and Reference))

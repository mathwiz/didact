(setq a '(a b))
(setq b 'x)
(set b 'y)

(print b)
(print x)


;; 1.5.1
;; compare to using setf
(setq a '(u v w))
(set (car (cdr a)) 'b)
(print (cons v a))


;; 1.5.2
(setq a '(u v w))
'(setq a '(x y z))
(print a)


;; 1.5.3
(setq a 'a)
(setq b 'a)
(print (list a b 'b))


;; 1.5.4
(print (list (list 'a 'b) '(list 'a 'b)))


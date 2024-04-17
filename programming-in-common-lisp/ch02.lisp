;; ************************************************
(print '(2.1 Appending Two Lists))

(setq subject1 '(the cow))
(setq subject2 '(the quick brown fox))
(setq predicate1 '(jumped over the moon))
(setq predicate2 '(jumped over the lazy dog))

(defun my-append (x y)
  (if (null x)
      y
      (cons (car x) (my-append (cdr x) y))))

(my-append subject1 predicate1)
(my-append subject2 predicate2)
(my-append subject2 predicate1)

(setq x '(r s t))

(setq y '(u v w))

(setq a (my-append x y))
(setq b (my-append x y))



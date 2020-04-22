;; p1
(defun dot-product (trip1 trip2)
  (+ (* (car trip1) (car trip2))
     (* (car (cdr trip1)) (car (cdr trip2)))
     (* (car (cdr (cdr trip1))) (car (cdr (cdr trip2))))))

(print 'p1)
(print (dot-product '(1.2 2.4 -0.3) '(0.0 4.5 3.8)))
(print (dot-product '(1 3 -5) '(4 -2 -1)))


;; p2
(defun count-numbers (lst)
  (if (null lst)
      0
      (if (numberp (car lst))
          (+ 1 (count-numbers (cdr lst)))
          (count-numbers (cdr lst)))))

(print 'p2)
(print (count-numbers '(a 2.3 b 5 4.53 c d)))
(print (count-numbers '(no numbers in this list!)))


;; p3

(print 'p1)
(defun dot-product (trip1 trip2)
  (+ (* (car trip1) (car trip2))
     (* (car (cdr trip1)) (car (cdr trip2)))
     (* (car (cdr (cdr trip1))) (car (cdr (cdr trip2))))))

(print (dot-product '(1.2 2.4 -0.3) '(0.0 4.5 3.8)))
(print (dot-product '(1 3 -5) '(4 -2 -1)))
(print (dot-product '(1 1 1) '(1 2 3)))


(print 'p2)
(defun count-numbers (lst)
  (if (null lst)
      0
      (if (numberp (car lst))
          (+ 1 (count-numbers (cdr lst)))
          (count-numbers (cdr lst)))))

(print (count-numbers '(a 2.3 b 5 4.53 c d)))
(print (count-numbers '(no numbers in this list!)))


(print 'p3)
(defun longer-listp (longer other)
  (> (length longer) (length other)))

(print (longer-listp nil nil))
(print (longer-listp 'nil '(1)))
(print (longer-listp '(1) '(1)))
(print (longer-listp '(1 2) '(1 2 3)))
(print (longer-listp '(1) nil))
(print (longer-listp '(1 2) '(1)))


(print 'p4)
(defun same-length (lst)
  (if (null lst)
      nil
      (cons 't (same-length (cdr lst)))))

(print (same-length nil))
(print (same-length '(1)))
(print (same-length (list 1 2 3)))


(print 'p5)
(defun new-list (n)
  (if (<= n 0)
      nil
      (cons 't (new-list (- n 1)))))

(print (new-list -1))
(print (new-list 0))
(print (new-list 1))
(print (new-list 21))

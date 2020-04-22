;; 1
(defun dot-product (trip1 trip2)
  (+ (* (car trip1) (car trip2))
     (* (car (cdr trip1)) (car (cdr trip2)))
     (* (car (cdr (cdr trip1))) (car (cdr (cdr trip2))))))


(print (dot-product '(1.2 2.4 -0.3) '(0.0 4.5 3.8)))
(print (dot-product '(1 3 -5) '(4 -2 -1)))


;; 2

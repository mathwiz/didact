(defun count-up (n)
  (labels ((recurse (cnt)
	     (cond ((> cnt n) nil)
		   (t (cons cnt (recurse (+ cnt 1)))))))
    (recurse 1)))


(defun count-up-tail (n)
  (labels ((recurse (cnt acc)
	     (cond ((< cnt 1) acc)
		   (t (recurse (- cnt 1) (cons cnt acc))))))
    (recurse n nil)))


(defun fact (n)
  (cond ((<= n 0) 1)
	(t (* n (fact (1- n))))))


(defun fact-tail (n)
  (labels ((recurse (cnt acc)
                    (cond ((<= cnt 0) acc)
                          (t (recurse (1- cnt) (* cnt acc))))))
    (recurse n 1)))


(defun union-tail (x y)
  (labels ((recurse (left right)
                    (cond ((null right) left)
                          ((member (car right) left) (recurse left (cdr right)))
                          (t (recurse (cons (car right) left) (cdr right))))))
    (recurse x y)))


(defun intersection-tail (x y)
  (labels ((recurse (left right)
                    (cond ((null right) left)
                          ((member (car right) left) (recurse left (cdr right)))
                          (t (recurse (cdr left) (cdr right))))))
    (recurse x y)))


(defun set-difference-tail (x y)
  (labels ((recurse (left right acc)
                    (cond ((null left) acc)
                          ((member (car left) right) (recurse (cdr left) right acc))
                          (t (recurse (cdr left) right (cons (car left) acc))))))
    (recurse x y nil)))



;; Test cases
(defvar x)
(defvar y)
(setf x (list 1 2 3 4 5))
(setf y (list 4 5 6 7 8))
;; (union-tail x y)
;; (3 2 1 4 5 6 7 8)
;; (intersection-tail x y)
;; (5 4)
;; (set-difference-tail x y)
;; (3 2 1)
;; (set-difference-tail y x)
;; (8 7 6)



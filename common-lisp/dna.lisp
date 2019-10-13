(defun complement-base (b)
  (cond ((equal b 'a) 't)
	((equal b 't) 'a)
	((equal b 'g) 'c)
	((equal b 'c) 'g)
	(t (error "Bad argument"))))

(defun complement-strand (strand)
  (mapcar #'complement-base strand))

(defun make-double (strand)
  (mapcar #'(lambda (x) (list x (complement-base x))) strand))


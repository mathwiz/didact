(defun complement-base (b)
  (cond ((equal b 'a) 't)
	((equal b 't) 'a)
	((equal b 'g) 'c)
	((equal b 'c) 'g)
	(t (error "Bad argument"))))


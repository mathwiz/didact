(defmacro our-and (&rest args)
  (case (length args)
    (0 t)
    (1 (car args))
    (t `(if ,(car args)
	    (our-and ,@(cdr args))))))


(defmacro our-andb (&rest args)
  (if (null args)
      t
      (labels ((expander (rest)
		 (if (cdr rest)
		     `(if ,(car rest)
			  ,(expander (cdr rest)))
		     (car rest))))
	(expander args))))


;; Test

(our-and 1 2 3)
(our-and 1 nil 2)

(our-andb 1 2 3)
(our-andb 1 nil 2)


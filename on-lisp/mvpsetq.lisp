(defmacro mvpsetq (&rest args)
  (let* ((pairs (group args 2))
	 (syms (mapcar #'(lambda (p)
			   (mapcar #'(lambda (x) (gensym))
				   (mklist (car p))))
		       pairs)))
    (labels ((rec (ps ss)
	       (if (null ps)
		   `(setq
		     ,@(mapcan #'(lambda (p s)
				   (shuffle (mklist (car p))
					    s))
			       pairs syms))
		   (let ((body (rec (cdr ps) (cdr ss))))
		     (let ((var/s (caar ps))
			   (expr (cadar ps)))
		       (if (consp var/s)
			   `(multiple-value-bind ,(car ss)
				,expr
			      ,body)
			   `(let ((,@(car ss) ,expr))
			      ,body)))))))
      (rec pairs syms))))


(defun shuffle (x y)
  (cond ((null x) y)
	((null y) x)
	(t (list* (car x) (car y)
		  (shuffle (cdr x) (cdr y))))))

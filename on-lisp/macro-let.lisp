(defmacro our-let (binds &body body)
  `((lambda ,(mapcar #'(lambda (x)
			 (if (consp x) (car x) x))
		     binds)
      ,@body)
    ,@(mapcar #'(lambda (x)
		  (if (consp x) (cadr x) nil))
	      binds)))



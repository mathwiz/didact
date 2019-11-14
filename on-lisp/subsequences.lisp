(defmacro do-tuples/o (parms source &body body)
  (if parms
      (let ((src (gensym)))
	`(prog ((,src ,source))
	    (mapc #'(lambda ,parms ,@body)
		  ,@(map0-n #'(lambda (n)
				`(nthcdr ,n ,src))
			    (1- (length parms))))))))


(defmacro do-tuples/c (parms source &body body)
  (if parms
      (with-gensyms (src rest bodfn)
	(let ((len (length parms)))
	  `(let ((,src ,source))
	     (when (nthcdr ,(1- len) ,src)
	       (labels ((,bodfn ,parms ,@body))
		 (do ((,rest ,src (cdr ,rest)))
		     ((not (nthcdr ,(1- len) ,rest))
		      ,@(mapcar #'(lambda (args)
				    `(,bodfn ,@args))
				(dt-args len rest src))
		      nil)
		   (,bodfn ,@map1-n #'(lambda (n)
					`(nth ,(1- n)
					      ,rest))
			   len)))))))))


(defun dt-args (len rest src)
  (map0-n #'(lambda (n)
	      (map1-n #'(lambda (n)
			  (let ((x (+ m n)))
			    (if (>= x len)
				`(nth ,(- x len) ,src)
				`(nth ,(1- x) ,rest))))
		      len))
	  (- len 2))) 


;; Example
(do-tuples/c (x y z) '(a b c d) (princ (list x y z)))

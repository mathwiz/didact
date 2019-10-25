;; requires mappend from mapping.lisp
(defmacro condlet (clauses &body body)
  (let ((bodfn (gensym))
	(vars (mapcar #'(lambda (v) (cons v (gensym)))
		      (remove-duplicates
		       (mapcar #'car
			       (mappend #'cdr clauses))))))
    `(labels ((,bodfn ,(mapcar #'car vars)
		,@body))
       (cond ,@(mapcar #'(lambda (cl)
			   (condlet-clause vars cl bodfn))
		       clauses)))))


(defun condlet-clause (vars cl bodfn)
  `(,(car cl) (let ,(mapcar #'cdr vars)
		(let ,(condlet-binds vars cl)
		  (,bodfn ,@(mapcar #'cdr vars))))))


(defun condlet-binds (vars cl)
  (mapcar #'(lambda (bindform)
	      (if (consp bindform)
		  (cons (cdr (assoc (car bindform) vars))
			(cdr bindform))))
	  (cdr cl)))


;; Test

(condlet (((= 1 2) (x (princ 'a)) (y (princ 'b)))
	  ((= 1 1) (y (princ 'c)) (x (princ 'd)))
	  (t       (x (princ 'e)) (z (princ 'f))))
  (list x y z))
;; expect output CD, value (D C NIL)
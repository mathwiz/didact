(defun dot-prin1 (objs)
   (cond ((listp (car objs)) (dot-prin1 (car objs)) (dot-prin1 (cdr objs)))
	 (t objs)))

(format t "~S"
	(format t "(")

	(format t ")"))
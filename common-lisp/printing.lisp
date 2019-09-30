(defun dot-prin1 (objs)
  (cond ((null objs) (format t "~S" nil))
	((listp objs)
	 (print-open)
	 (cond
	   ((listp (car objs)) (dot-prin1 (car objs)) (format t " . ") (dot-prin1 (cdr objs)))
	   (t (format t "~S . " (car objs)) (dot-prin1 (cdr objs))))
	 (print-close))
	(t (format t "~S" objs))))

(defun print-open ()
  (format t "("))

(defun print-close ()
  (format t ")"))

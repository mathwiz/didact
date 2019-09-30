(defun dot-prin1 (objs)
  (cond ((null objs) (format t "NIL"))
	((listp objs)
	 (print-open)
	 (dot-prin1 (car objs))
	 (print-dot)
	 (dot-prin1 (cdr objs))
	 (print-close))
	(t (format t "~S" objs))))

(defun hybrid-prin1 (objs)
  (cond ((null objs) (format t ""))
	((listp objs)
	 (print-open)
	 (hybrid-prin1 (car objs))
	 (print-dot)
	 (hybrid-prin1 (cdr objs))
	 (print-close))
	(t (format t "~S" objs))))



(defun print-open ()
  (format t "("))

(defun print-close ()
  (format t ")"))

(defun print-dot ()
  (format t " . "))
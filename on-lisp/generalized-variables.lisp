(defmacro allf (val &rest args)
  (with-gensyms (gval)
    `(let ((,gval ,val))
       (setf ,@(mapcan #'(lambda (a) (list a gval))
		       args)))))


(defmacro nilf (&rest args) `(allf nil ,@args))


(defmacro tf (&rest args) `(allf t ,@args))


(defmacro toggle (&rest args)
  `(progn
     ,@(mapcar #'(lambda (a) `(toggle2 ,a))
	       args)))


(define-modify-macro toggle2 () not)


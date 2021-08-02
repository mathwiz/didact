(defmacro let1 (var val &body body) 
  `(let ((,var ,val)) ,@body))


(let1 foo (+ 2 3) 
      (format t "lisp is awesome ~a" (gensym)) 
      (print (* foo foo)) 
      (format t "~%"))


(print 
 (macroexpand '(let1 foo (+ 2 3) 
        (princ (gensym)) 
        (print (* foo foo)) 
        (format t "~%")))
)


(format t "~%")

(defun verbose-add (a b) 
  (let1 x (+ a b) 
        (format t "The sum of ~a and ~a is ~a." a b x) x))


(verbose-add 5 6)

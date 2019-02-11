(defvar *N* 10 
  "Number of iterations")
(defparameter *SYM* '* "Symbol to print")
(defconstant *PI* 22/7 "Bad approximation of PI")

(let ((*N* 30)) 
  (print (symbol-value '*n*))
  (1+ *n*))

(let ((my-pi 3.14)) 
  (print (symbol-value '*pi*))
  (print my-pi))

(symbol-value '*pi*)
(symbol-value 'my-pi)  ; Fails

(defun init ()
  (defconstant *E* 2.7)
  (defconstant *PHI* 1.6))

(setf x 1)

(let ((x 5) (y (1+ x)))
  (print x)
  (print y)
  (print (+ x y))
  (setf x 0))

(let* ((x 5) (y (1+ x)))
  (print x)
  (print y)
  (print (+ x y))
  (setf x 0))

(print x)

(eval (list *sym* 2 3))



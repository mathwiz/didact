(defun funcall-example () 
  (setf f '+) 
  (setf g '*) 
  (print (funcall f 2 5)) 
  (print (funcall g 2 5))
  nil)


(defun apply-example () 
  (print (apply '- '(2 5))) 
  (print (apply (function -) 
                '(2 5)))
  (print (apply #'- '(2 5))) 
  (print (apply (function (lambda (x y) 
                  (- (* x x) 
                     (* y y)))) 
                '(2 5))) 
  (print (apply '- 10 1 '(2 5))) 
  (print (apply '- 10 1 2 5 '()))
  nil)

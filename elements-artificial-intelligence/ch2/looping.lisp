(defun loop-example () 
  (loop (print "Enter an S-exp 'bye' to exit") 
     (setf x (read))  ;; Get an S-exp from the user
     (if (eql x 'bye) ;; Test for terminatino
         (return)) 
     (setf x (cons x x)) 
     (print x)))


(defun dotimes-example () 
  (dotimes (it 10) 
    (print (expt 2 it))))


(defun dolist-example () 
  (dolist (it '(north south) state)
    (setf state (cons it '(dakota)))
    (print (append state '(is in the midwest)))))


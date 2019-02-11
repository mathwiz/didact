(setf too-young 16)

(defun non-minor-experience (x) 
  "Returns number of years of majority. If below 21, aborts computation of non-minor experience"
  (if (< x 21) 
      (throw 'bar-test 'too-young) 
      (- x 21)))

(catch 'bar-test 
  (setf age 18) 
  (setf y (non-minor-experience age)) 
  (cons y '(years of non-minor experience)))


(setf manipulate-fail -1)

(defun turn-on-motor () 
  (print "Turn on motor"))


(defun turn-off-motor () 
  (print "Turn off motor"))


(defun manipulate () 
  (print "Manipulate") 
  (throw 'robot-op 'manipulate-fail))


(catch 'robot-op
  (unwind-protect (progn (turn-on-motor) 
                         (manipulate)) 
    (turn-off-motor)))


(print "Loaded")


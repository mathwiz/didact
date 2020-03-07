(defparameter *small* 1)
(defparameter *big* 1000)
	
(defun new-game ()
	(setf *small* 1)
	(setf *big* 1000))

(defun guess-my-number ()
  (ash (+ *small* *big*) -1))


(defun smaller ()
  (setf *big* (- (guess-my-number) 1))
  (guess-my-number))


(defun bigger ()
  (setf *small* (+ (guess-my-number) 1))
  (guess-my-number))

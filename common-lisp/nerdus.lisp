(setf S 'sleeping)
(setf E 'eating)
(setf W 'waiting-for-a-computer)
(setf P 'programming)
(setf D 'debugging)

(setf NERD-STATES (list (list S E) 
                        (list E W) 
                        (list W P) 
                        (list P D) 
                        (list D S)))

(defun nerdus (state) 
  (cadr (assoc state NERD-STATES)))

(defun sleepless-nerd (state)
  (cond ((eq state D) (nerdus S))
        (t (nerdus state))))

(defun nerd-on-caffeine (state)
  (nerdus (nerdus state)))

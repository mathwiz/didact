(defun space-over (n) 
  (cond ((< n 0) 
         (format t "Error!"))
        ((= n 0) (format t ""))
        (t (format t " ") (space-over (- n 1)))))


(defun plot-one-point (plotting-string y-val)
  (space-over y-val)
  (format t plotting-string))


(defun plot-points (plotting-string points)
  (mapcar #'(lambda (x) 
              (plot-one-point plotting-string x)
              (format t "~%")) 
          points))


(defun generate (start stop)
  (cond ((> start stop) nil)
        (t (cons start (generate (+ start 1) stop)))))


(defun test (n) 
  (format t "-%>>>") 
  (space-over n) 
  (format t "<<<"))


(defun test2 (points)
  (plot-points "< >" points))

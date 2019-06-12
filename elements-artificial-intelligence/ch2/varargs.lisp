(defun mean (x1 &rest x-list)
  (/ (apply #'+ x1 x-list)
     (1+ (length x-list))))

(print (mean 2 3 4 5))

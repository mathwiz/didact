(defun mean (x1 &rest x-list)
  (/ (apply #'+ x1 x-list)
     (1+ (length x-list))))

(defun mean2 (&rest x-list)
  (/ (apply #'+ x-list)
     (length x-list)))

(print (mean 2 3 4 5))
(print (mean2 2 3 4 5))

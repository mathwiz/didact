(defvar *hist-array*)
(defvar *total-points*)


(defun new-histogram (n)
  (setf *hist-array* (make-array n :initial-element 0))
  (setf *total-points* n))


(defvar *hist-array*)
(defvar *total-points*)


(defun new-histogram (n)
  (setf *hist-array* (make-array n :initial-element 0))
  (setf *total-points* 0))


(defun record-value (n)
  (cond (nil nil)
        (t nil)))


(defun print-hist-line (n)
  nil)


(defun print-histogram ()
  (dotimes (i (length *hist-array*))
    (print-hist-line i)))


(defvar *hist-array*)
(defvar *total-points*)

(defun get-hist ()
  *hist-array*)


(defun new-histogram (n)
  (setf *hist-array* (make-array n :initial-element 0))
  (setf *total-points* 0))


(defun record-value (n)
  (let ((h *hist-array*))
    (cond ((or (> n 0) (< n (length h))) (setf (aref h n) (1+ (aref h n))))
          (t (error "Input must be between 0 and ~S" (length h))))))


(defun print-hist-line (n)
  (let ((val (aref *hist-array* n)))
    (progn
      (format t "~2S [~3S] " n val)
      (dotimes (i val)
        (format t "*")))))


(defun print-histogram ()
  (dotimes (i (length *hist-array*))
    (progn
      (print-hist-line i)
      (format t "~%")
      )))


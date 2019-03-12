;;; References ROOMS from room-map.lisp expected in env

(defun choices (room)
  (rest (assoc room ROOMS)))


(defun look (direction room)
  (let* ((dirs (choices room))
         (target (assoc direction dirs)))
    (if target
        (cadr target)
        nil)))
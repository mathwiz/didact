;;; References ROOMS from room-map.lisp expected in env

(defvar LOC)

(setf LOC 'pantry)


(defun set-robbie-location (place)
  (setf LOC place))


(defun choices (room)
  (rest (assoc room ROOMS)))


(defun look (direction room)
  (let* ((dirs (choices room))
         (target (assoc direction dirs)))
    (if target
        (cadr target)
        nil)))


(defun how-many-choices ()
  (length (choices LOC)))


(defun upstairs-p (room)
  (or (eq room 'library)
      (eq room 'upstairs-bedroom)))


(defun onstairs-p (room)
  (or (eq room 'front-stairs)
      (eq room 'back-stairs)))


(defun where ()
  (let ((start (list 'robbie 'is)))
    (append start
            (if (onstairs-p LOC)
                (list 'on 'the LOC)
                (list (if (upstairs-p LOC) 'upstairs 'downstairs)
                      'in 'the LOC)))))


(defun move (direction)
  (let ((target (look direction LOC)))
    (cond ((not target) (list 'ouch! 'robbie 'hit 'a 'wall))
          (t (set-robbie-location target) (where)))))

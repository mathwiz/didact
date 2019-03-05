(setf VS '-vs-)
(setf tester '(small red metal cube -vs- red plastic small cube))

(defun right-side (xs)
  (rest (member VS xs)))

(defun left-side (xs)
  (reverse (right-side (reverse xs))))

(defun count-common (xs)
  (length (intersection (right-side xs) (left-side xs))))

(defun compare (xs)
  (list (count-common xs) 'common 'features))



(defun bad-reverse (xs)
  (if (null xs)
      'nil
      (append (bad-reverse (cdr xs))
              (cons (car xs) 'nil))))

(defun reverse-aux (xs acc)
  (if (null xs)
      acc
      (reverse-aux (cdr xs) (cons (car xs) acc))))

(defun my-reverse (xs)
  (reverse-aux xs 'nil))

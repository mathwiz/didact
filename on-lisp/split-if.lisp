(defun split-if (fn lst)
  (let ((acc nil))
    (do ((src lst (cdr src)))
	((or (null src) (funcall fn (car src)))
	 (values (nreverse acc) src))
      (push (car src) acc))))

;; Usage: (split-if #'(lambda (x) (> x 4)) '(1 2 3 4 5 6 7 8 9 10))

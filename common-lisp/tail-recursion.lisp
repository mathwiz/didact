(defun count-up (n)
  (labels ((recurse (cnt)
	     (cond ((> cnt n) nil)
		   (t (cons cnt (recurse (+ cnt 1)))))))
    (recurse 1)))


(defun count-up-tail (n)
  (labels ((recurse (cnt acc)
	     (cond ((< cnt 1) acc)
		   (t (recurse (- cnt 1) (cons cnt acc))))))
    (recurse n nil)))


(defun fact (n)
  (cond ((= n 0) 1)
	(t n)))

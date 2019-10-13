(defun duplicate (obj lst &key (test #'eql))
  (member obj (cdr (member obj lst :test test))
	  :test test))

;; Usage: (duplicate 'a '(a b c a d))

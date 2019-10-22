(defmacro my-memq (obj lst)
  `(member ,obj ,lst :test #'eq))


(defmacro while (test &body body)
  `(do ()
       ((not ,test))
     ,@body))


;; Testing
(princ (my-memq 'a '(z a b)))

(format t "~%")

(defvar x)
(setf x 0)

(while (< x 5)
  (progn (princ x)
	 (setf x (1+ x))))



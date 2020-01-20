(defmacro while (test &body bod)
  `(do ()
       ((not ,test))
     ,@bod)
  )


;; Example

(defun next-power-of-two (n &aux (i 1))
  (while (< i n)
    (format t "~&Not ~S" i)
    (setf i (* i 2)))
  i
  )

(defmacro bad-for (var start stop &body body)
  `(do ((,var ,start (1+ ,var))
	(limit ,stop))
       ((> ,var limit))
     ,@body))


(defmacro better-for (var start stop &body body)
  `(do ((b #'(lambda (,var) ,@body))
	(count ,start (1+ count))
	(limit ,stop))
       ((> count limit))
     (funcall b count)))


(defmacro for (var start stop &body body)
  (let ((gstop (gensym)))
      `(do ((,var ,start (1+ ,var))
	    (,gstop ,stop))
	   ((> ,var ,gstop))
	 ,@body)))


;; Testing
(defun endl ()
  (format t "~%"))

(bad-for i 0 4 (princ i))

(endl)

;; infinite
;; (bad-for limit 0 4 (princ limit))

(for limit 0 4 (princ limit))

(defun readlist (&rest args)
  (values (read-from-string
	   (concatenate 'string "("
			(apply #'read-line args)
			")"))))

;; Usage: (readlist)

(defun prompt (&rest args)
  (apply #'format *query-io* args)
  (read *query-io*))

;; Usage: (prompt "Enter a number under ~A.>> " 100)

(defun break-loop (fn quit &rest args)
  (format *query-io* "Entering break-loop.~%")
  (loop
       (let ((in (apply #'prompt args)))
	 (if (funcall quit in)
	     (return)
	     (format *query-io* "~A~%" (funcall fn in))))))

;; Usage: (break-loop #'eval #'(lambda (x) (eq x :q)) ">> ")

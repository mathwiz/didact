(defun trec (rec &optional (base #'identity))
  (labels
      ((self (tree)
	 (if (atom tree)
	     (if (functionp base)
		 (funcall base tree)
		 base)
	     (funcall rec tree
		      #'(lambda ()
			  (self (car tree)))
		      #'(lambda ()
			  (if (cdr tree)
			      (self (cdr tree))))))))
    #'self))


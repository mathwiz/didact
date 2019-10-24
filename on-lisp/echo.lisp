(defmacro bad-echo (&rest args)
  `',(nconc args (list 'amen)))


(defun foo1 () (bad-echo x))


(defmacro echo (&rest args)
  `'(,@args amen))


(defun foo2 () (echo x))


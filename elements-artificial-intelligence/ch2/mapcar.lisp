(mapcar (function 1+) '(5 10 7 -2 101))

(mapcar #'list '(a b c d) '(1 2 3 4))

(mapcar #'(lambda (word)
	    (cond ((eql word 'is) 'was)
		  (t word)))
	'(mt st helens is an active volcano))


(defun complement-base (b)
  (cond ((equal b 'a) 't)
	((equal b 't) 'a)
	((equal b 'g) 'c)
	((equal b 'c) 'g)
	(t (error "Bad argument"))))


(defun complement-strand-applicative (strand)
  (mapcar #'complement-base strand))


(defun complement-strand (strand)
  (let ((result nil))
    (dolist (x (reverse strand) result)
      (push (complement-base x) result))))


(defun make-double-applicative (strand)
  (mapcar #'(lambda (x) (list x (complement-base x))) strand))


(defun make-double (strand)
  (let ((result nil))
    (dolist (x (reverse strand) result)
      (push (list x (complement-base x)) result))))


(defun count-bases (nucleic-acid)
  (let ((all-bases (cond ((null nucleic-acid) nil)
			 ((listp (car nucleic-acid))
			  (append (mapcar #'first nucleic-acid)
				  (mapcar #'second nucleic-acid)))
			 (t nucleic-acid)))
	(na 0)
	(nt 0)
	(ng 0)
	(nc 0))
    (do* ((d all-bases (rest d)))
	((null d) (list (list 'a na)
			(list 't nt)
			(list 'g ng)
			(list 'c nc)))
      (cond ((eq (car d) 'a) (incf na 1))
	    ((eq (car d) 't) (incf nt 1))
	    ((eq (car d) 'g) (incf ng 1))
	    ((eq (car d) 'c) (incf nc 1))))))


(defun prefixp (a b)
  (do ((ai a (rest ai))
       (bi b (rest bi)))
      ((or (null ai) (null bi))
       (return (not ai)))
  (unless (eq (car ai) (car bi))
    (return nil))))


(defun appearsp (a b)
  (cond ((null a) t)
	(t
	 (do ((bi b (rest bi)))
	     ((null bi)
	      (return nil))
	   (when (prefixp a bi)
	     (return t))))))


(defun coverp (a b)
  (do ((ai a (if (null (rest ai))
		 a
		 (rest ai)))
       (bi b (rest bi)))
      ((null bi) (return (eq ai a)))
  (unless (eq (car ai) (car bi))
    (return nil))))


(defun prefix (n strand)
  (do ((counter n (1- counter))
       (s strand (rest s))
       (pre nil (cons (car s) pre)))
      ((eq counter 0) (return (reverse pre)))
    (when (null s)
      (return (reverse pre)))))


(defun kernel (strand)
  (if (null strand)
      nil
      (let ((len (length strand)))
	(do* ((n 1 (1+ n))
	      (cand (prefix n strand) (prefix n strand)))
	     ((eq n len) (return cand))
	  (when (coverp cand strand)
	    (return cand))))))


(defun base-as-string (b)
  (concatenate 'string "  " (symbol-name b) "  "))


(defun draw-dna (strand)
  (if (null strand)
      nil
      (let ((backbone "-----")
	    (connect  "  !  ")
	    (hydrogen "  .  ")
	    (len (length strand)))
	(do* ((n 1 (1+ n))
	      (s strand (rest s))
	      (main (base-as-string (car s))
		    (concatenate 'string main (base-as-string (car s))))
	      (comp (base-as-string (complement-base (car s)))
		    (concatenate 'string comp (base-as-string (complement-base (car s)))))
	      (back backbone (concatenate 'string back backbone))
	      (conn connect  (concatenate 'string conn connect))
	      (hydr hydrogen (concatenate 'string hydr hydrogen)))
	     ((eq n len) (progn
			   (format t "~A~%~A~%~A~%~A~%~A~%~A~%~A~%~A~%"
				   back conn main hydr hydr comp conn back)
			   (return nil)))
	  ))))

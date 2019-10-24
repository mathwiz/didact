(defmacro our-do (bindforms (test &rest result) &body body)
  (let ((label (gensym)))
    `(prog ,(make-initforms bindforms)
	,label
	(if ,test
	    (return (progn ,@result)))
	,@body
	(psetq ,@(make-stepforms bindforms))
	(go ,label))))


(defun make-initforms (bindforms)
  (mapcar #'(lambda (b)
	      (if (consp b)
		  (list (car b) (cadr b))
		  (list b nil)))
	  bindforms))


(defun make-stepforms (bindforms)
  (mapcan #'(lambda (b)
	      (if (and (consp b) (third b))
		  (list (car b) (third b))
		  nil))
	  bindforms))


;; Test
(let ((lst '(a b c d e)))
  (our-do ((s lst (rest s)))
      ((null s) (return nil))
    (prin1 (car s))))

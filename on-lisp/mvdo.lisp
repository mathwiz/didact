(defmacro mvdo* (parm-cl test-cl &body body)
  (mvdo-gen parm-cl parm-cl test-cl body))


(defun mvdo-gen (binds rebinds test body)
  (if (null binds)
      (let ((label (gensym)))
	`(prog nil
	    ,label
	    (if ,(car test)
		(return (progn ,@(cdr test))))
	    ,@body
	    ,@(mvdo-rebind-gen rebinds)
	    (go ,label)))
      (let ((rec (mvdo-gen (cdr binds) rebinds test body)))
	(let ((var/s (caar binds)) (expr (cadar binds)))
	  (if (atom var/s)
	      `(let ((,var/s ,expr)) ,rec)
	      `(multiple-value-bind ,var/s ,expr ,rec))))))


(defun mvdo-rebind-gen (rebinds)
  (cond ((null rebinds) nil)
	((< (length (car rebinds)) 3)
	 (mvdo-rebind-gen (cdr rebinds)))
	(t
	 (cons (list (if (atom (caar rebinds))
			 'setq
			 'multiple-value-setq)
		     (caar rebinds)
		     (third (car rebinds)))
	       (mvdo-rebind-gen (cdr rebinds))))))


(defmacro mvdo (binds (test &rest result) &body body)
  (let ((label (gensym))
	(temps (mapcar #'(lambda (b)
			   (if (listp (car b))
			       (mapcar #'(lambda (x) (gensym))
				       (car b))
			       (gensym)))
		       binds)))
    `(let ,(mappend #'mklist temps)
       (mvpsetq ,@(mapcan #'(lambda (b var)
			      (list var (cadr b)))
			  binds
			  temps))
       (prog ,(mapcar #'(lambda (b var) (list b var))
		      (mappend #'mklist (mapcar #'car binds))
		      (mappend #'mklist temps))
	  ,label
	  (if ,test
	      (return (progn ,@result)))
	  ,@body
	  (mvsetq ,@(mapcan #'(lambda (b)
				(if (third b)
				    (list (car b)
					  (third b))))
			    binds))
	  (go ,label)))))


;; ** Examples **

;; Squash using mvdo*
;; Assume referenced functions are defined
(defun clear () nil)
(defun draw (obj) nil)
(defun move (obj x y) nil)
(defun touch (obj1 obj2) nil)
(defun mouse-vector () nil)
(defun pos (obj) nil)

(mvdo* (((px py) (pos player)   (move player mx my))
	((x1 y1) (pos obj1)     (move obj1
				      (- px x1)
				      (- py y1)))
	((x2 y2) (pos obj2)     (move obj2
				      (- px x2)
				      (- py y2)))
	((mx my) (mouse-vector) (mouse-vector))
	(win     nil            (touch obj1 obj2))
	(lose    nil            (and (touch obj1 player)
				     (touch obj2 player))))
       ((or win lose) (if win 'win 'lose))
       (clear)
       (draw obj1)
       (draw obj2)
       (draw player))


;; mvdo
(mvdo ((x 1 (1+ x))
       ((y z) (values 0 0) (values z x)))
      ((> x 5) (list x y z))
      (princ (list x y z)))
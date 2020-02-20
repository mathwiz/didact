;; Built-in descructuring-bind
(destructuring-bind (x y z) '(1 2 3)
  (progn
    (print x)
    (print y)
    (print z)))


;; Same concept but works with any kind of sequence
(defmacro dbind (pat seq &body body)
  (let ((gseq (gensym)))
    `(let ((,gseq ,seq))
       ,(dbind-ex (destruc pat gseq #'atom) body))))


(defun destruc (pat seq &optional (atom? #'atom) (n 0))
  (if (null pat)
      nil
      (let ((rest (cond ((funcall atom? pat) pat)
                        ((eq (car pat) '&rest) (cadr pat))
                        ((eq (car pat) '&body) (cadr pat))
                        (t nil))))
        (if rest
            `((,rest (subseq ,seq ,n)))
            (let ((p (car pat))
                  (rec (destruc (cdr pat) seq atom? (1+ n))))
              (if (funcall atom? p)
                  (cons `(,p (elt ,seq ,n))
                        rec)
                  (let ((var (gensym)))
                    (cons (cons `(,var (elt ,seq ,n))
                                (destruc p var atom?))
                          rec))))))))


(defun dbind-ex (binds body)
  (if (null binds)
      `(progn ,@body)
      `(let ,(mapcar #'(lambda (b)
                         (if (consp (car b))
                             (car b)
                             b))
                     binds)
         ,(dbind-ex (mapcan #'(lambda (b)
                               (if (consp (car b))
                                   (cdr b)))
                           binds)
                   body))))


(dbind (a (b c) d) '(1 #(2 3) 4)
       (progn
         (print a)
         (print b)
         (print c)
         (print d)))


(dbind (a (b . c) &rest d) '(1 "fribble" 2 3 4)
       (progn
         (print a)
         (print b)
         (print c)
         (print d)))

(print (destruc '(a b c) 'seq #'atom))


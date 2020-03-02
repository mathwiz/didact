;; Require
(load "continuation-passing.lisp")


;; Tree traversal macros
(defun dft (tree)
  (cond ((null tree) nil)
        ((atom tree) (princ tree))
        (t (dft (car tree))
           (dft (cdr tree)))))


(setq *saved* nil)


(=defun dft-node (tree)
        (cond ((null tree) (restart))
              ((atom tree) (=values tree))
              (t (push #'(lambda () (dft-node (cdr tree)))
                       *saved*)
                 (dft-node (car tree)))))


(=defun restart ()
        (if *saved*
            (funcall (pop *saved*))
            (=values 'done)))


(=defun dft2 (tree)
        (setq *saved* nil)
        (=bind (node) (dft-node tree)
               (cond ((eq node 'done) (=values nil))
                     (t (princ node)
                        (restart)))))


;; Testing
(setq t1 '(a (b (d h)) (c e (f i) g))
      t2 '(1 (2 (3 6 7) 4 5)))

(format t "~%t1~%")
(print (dft2 t1))

(format t "~%t2~%")
(print (dft2 t2))


(print 'continuation-passing)

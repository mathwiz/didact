;; Require tree-recursor2.lisp, single.lisp
(load "tree-recursor2.lisp")
(load "single.lisp")

(defmacro atrec (rec &optional (base 'it))
  (let ((lfn (gensym)) (rfn (gensym)))
    `(trec #'(lambda (it ,lfn ,rfn)
               (symbol-macrolet ((left (funcall ,lfn))
                                 (right (funcall ,rfn)))
                 ,rec))
           #'(lambda (it) ,base))))


(defmacro on-trees (rec base &rest trees)
  `(funcall (atrec ,rec ,base) ,@trees))


;;;; Examples

(defun our-copy-tree (tree)
  (on-trees (cons left right) it tree))


(defun count-leaves (tree)
  (on-trees (+ left (or right 1)) 1 tree))


(defun flatten (tree)
  (on-trees (nconc left right) (mklist it) tree))


(defun rfind-if (fn tree)
  (on-trees (or left right)
            (and (funcall fn it) it)
            tree))


;; Require
(load "pattern-matching.lisp")
(load "continuation-passing.lisp")
(load "nondeterminism.lisp")


(defmacro with-inference (query &body body)
  `(progn
     (setq *paths* nil)
     (=bind (binds) (prove-query ',(rep_ query) nil)
            (let ,(mapcar #'(lambda (v)
                              `(,v (fullbind ',v binds)))
                          (vars-in query #'atom))
              ))))



;; Require
(load "generalized-variables.lisp")
(load "pattern-matching.lisp")
(load "continuation-passing.lisp")
(load "nondeterminism.lisp")


;; Top-level macro
(defmacro with-inference (query &body body)
  `(progn
     (setq *paths* nil)
     (=bind (binds) (prove-query ',(rep_ query) nil)
            (let ,(mapcar #'(lambda (v)
                              `(,v (fullbind ',v binds)))
                          (vars-in query #'atom))
              ,@body
              (fail)))))


(defun rep_ (x)
  (if (atom x)
      (if (eq x '_) (gensym "?") x)
      (cons (rep_ (car x)) (rep_ (cdr x)))))


(defun fullbind (x b)
  (cond ((varsym? x) (aif2 (binding x b)
                           (fullbind it b)
                           (gensym)))
        ((atom x) x)
        (t (cons (fullbind (car x) b)
                 (fullbind (cdr x) b)))))


(defun varsym? (x)
  (and (symbolp x) (eq (char (symbol-name x) 0) #\?)))


;; Interpretation of queries
(=defun prove-query (expr binds)
        (case (car expr)
          (and (prove-and (cdr expr) binds))
          (or (prove-or (cdr expr) binds))
          (not (prove-not (cadr expr) binds))
          (t (prove-simple expr binds))))


(=defun prove-and (clauses binds)
        (if (null clauses)
            (=values binds)
            (=bind (binds) (prove-query (car clauses) binds)
                   (prove-and (cdr clauses) binds))))


(=defun prove-or (clauses binds)
        (choose-bind c clauses
                     (prove-query c binds)))


(=defun prove-not (expr binds)
        (let ((save-paths *paths*))
          (setq *paths* nil)
          (choose (=bind (b) (prove-query expr binds)
                         (setq *paths* save-paths)
                         (fail))
                  (progn
                    (setq *paths* save-paths)
                    (=values binds)))))


(=defun prove-simple (query binds)
        (choose-bind r *rlist*
                     (implies r query binds)))


;; Rules
(defvar *rlist* nil)


(defmacro <- (con &rest ant)
  (let ((ant (if (= (length ant) 1)
                 (car ant)
                 `(and ,@ant))))
    `(length (conc1f *rlist* (rep_ (cons ',ant ',con))))))


(=defun implies (r query binds)
        (let ((r2 (change-vars r)))
          (aif2 (match query (cdr r2) binds)
                (prove-query (car r2) it)
                (fail))))


(defun change-vars (r)
  (sublis (mapcar #'(lambda (v)
                      (cons v (symb '? (gensym))))
                  (vars-in r #'atom))
          r))



;; Require
(load "generalized-variables.lisp")
(load "pattern-matching.lisp")
(load "continuation-passing.lisp")
(load "nondeterminism.lisp")


;; Top-level macro
(defmacro with-inference (query &body body)
  (let ((vars (vars-in query #'simple?)) (gb (gensym)))
    `(with-gensyms ,vars
       (setq *paths* nil)
       (=bind (,gb) ,(gen-query (rep_ query))
              (let ,(mapcar #'(lambda (v)
                                `(,v (fullbind ,v ,gb)))
                            vars)
                ,@body)
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
  (and (symbolp x) (not (symbol-package x))))


;; Interpretation of queries
(defun gen-query (expr &optional binds)
  (case (car expr)
    (and (gen-and (cdr expr) binds))
    (or (gen-or (cdr expr) binds))
    (not (gen-not (cadr expr) binds))
    (t `(prove (list ',(car expr)
                     ,@ (mapcar #'form (cdr expr)))
               ,binds))))


(defun gen-and (clauses binds)
  (if (null clauses)
      `(=values ,binds)
      (let ((gb (gensym))) 
        `(=bind (,gb) ,(gen-query (car clauses) binds)
                (gen-and (cdr clauses) binds)))))


(defun gen-or (clauses binds)
  `(choose 
    ,@(mapcar #'(lambda (c) (gen-query c binds))
              clauses)))


(defun gen-not (expr binds)
  (let ((gpaths (gensym)))
    `(let ((,gpaths *paths*))
       (setq *paths* nil)
       (choose (=bind (b) ,(gen-query expr binds)
                      (setq *paths* ,gpaths)
                      (fail))
                  (progn
                    (setq *paths* ,gpaths)
                    (=values ,binds))))))


(=defun prove (query binds)
        (choose-bind r *rlist* (=funcall r query binds)))


(defun form (pat)
  (if (simple? pat)
      pat
      `(cons ,(form (car pat)) ,(form (cdr pat)))))


;; Rules
(defvar *rules* nil)


(defmacro <- (con &rest ant)
  (let ((ant (if (= (length ant) 1)
                 (car ant)
                 `(and ,@ant))))
    `(length (conc1f *rules* ,(rule-fn (rep_ ant) (rep_ con))))))


(defun rule-fn (ant con)
  (with-gensyms (val win fact binds)
    `(=lambda (,fact ,binds)
              (with-gensyms ,(vars-in (list ant con) #'simple?)
                (multiple-value-bind (,val ,win)
                    (match ,fact
                           (list ',(car con)
                                 ,@ (mapcar #'form (cdr con)))
                           ,binds)
                  (if ,win
                      ,(gen-query ant val)
                      (fail)))))))



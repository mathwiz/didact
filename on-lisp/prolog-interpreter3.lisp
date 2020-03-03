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
       (=bind (,gb) ,(gen-query (rep_ query) nil '*paths*)
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
(defun gen-query (expr binds paths)
  (case (car expr)
    (and (gen-and (cdr expr) binds paths))
    (or (gen-or (cdr expr) binds paths))
    (not (gen-not (cadr expr) binds paths))
    (lisp (gen-lisp (cadr expr) binds))
    (is (gen-is (cadr expr) (third expr) binds))
    (cut `(progn (setq *paths* ,paths)
                 (=values ,binds)))
    (t `(prove (list ',(car expr)
                     ,@ (mapcar #'form (cdr expr)))
               ,binds *paths*))))


(defun gen-and (clauses binds paths)
  (if (null clauses)
      `(=values ,binds)
      (let ((gb (gensym))) 
        `(=bind (,gb) ,(gen-query (car clauses) binds paths)
                ,(gen-and (cdr clauses) gb paths)))))


(defun gen-or (clauses binds paths)
  `(choose 
    ,@(mapcar #'(lambda (c) (gen-query c binds paths))
              clauses)))


(defun gen-not (expr binds paths)
  (let ((gpaths (gensym)))
    `(let ((,gpaths *paths*))
       (setq *paths* nil)
       (choose (=bind (b) ,(gen-query expr binds paths)
                      (setq *paths* ,gpaths)
                      (fail))
                  (progn
                    (setq *paths* ,gpaths)
                    (=values ,binds))))))


(defmacro with-binds (binds expr)
  `(let ,(mapcar #'(lambda (v) `(,v (fullbind ,v ,binds)))
                 (vars-in expr))))


(defun gen-lisp (expr binds)
  `(if (with-binds ,binds ,expr)
       (=values ,binds)
       (fail)))


(defun gen-is (expr1 expr2 binds)
  `(aif2 (match ,expr1 (with-binds ,binds ,expr2) ,binds)
         (=values it)
         (fail)))


(=defun prove (query binds paths)
        (choose-bind r *rules* (=funcall r query binds paths)))


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
  (with-gensyms (val win fact binds paths)
    `(=lambda (,fact ,binds ,paths)
              (with-gensyms ,(vars-in (list ant con) #'simple?)
                (multiple-value-bind (,val ,win)
                    (match ,fact
                           (list ',(car con)
                                 ,@ (mapcar #'form (cdr con)))
                           ,binds)
                  (if ,win
                      ,(gen-query ant val)
                      (fail)))))))



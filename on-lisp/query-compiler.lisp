;; Require
(load "pattern-matching.lisp")


;; Database
(defun make-db (&optional (size 100))
  (make-hash-table :size size))


(defvar *default-db* (make-db))


(defun clear-db (&optional (db *default-db*))
  (clrhash db))


(defmacro db-query (key &optional (db '*default-db*))
  `(gethash ,key ,db))


(defun db-push (key val &optional (db *default-db*))
  (push val (db-query key db)))


(defmacro fact (pred &rest args)
  `(progn (db-push ',pred ',args)
          ',args))


;; Query interpreter
(defmacro with-answer (query &body body)
  (let ((binds (gensym)))
    `(dolist (,binds (interpret-query ',query))
       (let ,(mapcar #'(lambda (v)
                         `(,v (binding ',v ,binds)))
                     (vars-in query #'atom))
         ,@body))))


(defun interpret-query (expr &optional binds)
  (case (car expr)
    (and (interpret-and (reverse (cdr expr)) binds))
    (or  (interpret-or (cdr expr) binds))
    (not (interpret-not (cadr expr) binds))
    (t   (lookup (car expr) (cdr expr) binds))))


(defun interpret-and (clauses binds)
  (if (null clauses)
      (list binds)
      (mapcan #'(lambda (b)
                  (interpret-query (car clauses) b))
              (interpret-and (cdr clauses) binds))))


(defun interpret-or (clauses binds)
  (mapcan #'(lambda (c)
              (interpret-query c binds))
          clauses))


(defun interpret-not (clause binds)
  (if (interpret-query clause binds)
      nil
      (list binds)))


(defun lookup (pred args &optional binds)
  (mapcan #'(lambda (x)
              (aif2 (match x args binds) (list it)))
          (db-query pred)))



;; Testing
(clear-db)

(fact paiter hogarth william english)
(fact painter reynolds joshua english)
(fact painter canale antonio venetian)
(fact dates hogarth 1697 1772)
(fact dates canale 1697 1768)
(fact dates reynolds 1723 1792)


(print (lookup 'painter '(?x ?y english)))

(let ((result (interpret-query '(and (painter ?x ?y ?z)
                                 (dates ?x 1697 ?w)))))
  (princ result))


(print 'query-compiler)

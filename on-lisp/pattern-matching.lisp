;; Requires
(load "anaphoric.lisp")
(load "anaphoric-mult-value.lisp")


(defun match (x y &optional binds)
  (acond2 
   ((or (eql x y) (eql x '_) (eql y '_)) (values binds t))
   ((binding x binds) (match it y binds))
   ((binding y binds) (match x it binds))
   ((varsym? x) (values (cons (cons x y) binds) t))
   ((varsym? y) (values (cons (cons y x) binds) t))
   ((and (consp x) (consp y) (match (car x) (car y) binds))
    (match (cdr x) (cdr y) it))
   (t (values nil nil))))


(defun varsym? (x)
  (and (symbolp x) (eq (char (symbol-name x) 0) #\?)))


(defun binding (x binds)
  (labels ((recbind (x binds)
             (aif (assoc x binds)
                  (or (recbind (cdr it) binds)
                      it))))
    (let ((b (recbind x binds)))
      (values (cdr b) b))))


(defmacro if-match (pat seq then &optional else)
  `(aif2 (match ',pat ,seq)
         (let ,(mapcar #'(lambda (v)
                           `(,v (binding ',v it)))
                       (vars-in then #'atom))
           ,then)
         ,else))


(defun vars-in (expr &optional (atom? #'atom))
  (if (funcall atom? expr)
      (if (var? expr) (list expr))
      (union (vars-in (car expr) atom?)
             (vars-in (cdr expr) atom?))))


(defun var? (x)
  (and (symbolp x) (eq (char (symbol-name x) 0 #\?))))



;; examples
(print (match '(p a b c a) '(p ?x ?y c ?x)))

(let ((m (match '(p ?x b ?y a) '(p ?y b c a))))
  (print m))

(let ((m (match '(a b c) '(a a a))))
  (print m))

(let ((m (match '(a ?x b) '(_ 1 _))))
  (print m))



(print 'pattern-matching)

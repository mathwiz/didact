;; Require
(load "iteration-macros.lisp")
(load "strings.lisp")

;; Built-in descructuring-bind
(destructuring-bind (x y z) '(1 2 3)
  (progn
    (print (list x y z))))


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


;; examples
(dbind (a (b c) d) '(1 #(2 3) 4)
       (progn
         (print (list a b c d))))


(dbind (a (b . c) &rest d) '(1 "fribble" 2 3 4)
       (progn
         (print (list a b c d))))

(print (destruc '(a b c) 'seq #'atom))



;; Destructuring on arrays
(defmacro with-matrix (pats ar &body body)
  (let ((gar (gensym)))
    `(let ((,gar ,ar))
       (let ,(let ((row -1))
               (mapcan
                #'(lambda (pat)
                    (incf row)
                    (setq col -1)
                    (mapcar #'(lambda (p)
                                `(,p (aref ,gar
                                           ,row
                                           ,(incf col))))
                            pat))
                pats))
         ,@body))))


(defmacro with-array (pat ar &body body)
  (let ((gar (gensym)))
    `(let ((,gar ,ar))
       (let ,(mapcar #'(lambda (p)
                         `(,(car p) (aref ,gar ,@ (cdr p))))
                     pat)
         ,@body))))


;; examples
(setq ar (make-array '(3 3)))

(for (r 0 2)
     (for (c 0 2)
          (setf (aref ar r c) (+ (* r 10) c))))

(with-matrix ((a b c)
              (d e f)
              (g h i)) ar
              (print (list a b c d e f g h i)))

(with-array ((a 0 0) (d 1 1) (i 2 2)) ar
            (print (list a d i)))


;; Destructuring on structures
(defmacro with-struct ((name . fields) struct &body body)
  (let ((gs (gensym)))
    `(let ((,gs ,struct))
       (let ,(mapcar #'(lambda (f)
                         `(,f (,(symb name f) ,gs)))
                     fields)
         ,@body))))


;; examples
(defstruct visitor name title firm)

(setq theo (make-visitor :name "Theodebert"
                         :title 'king
                         :firm 'franks))

(with-struct (visitor- name firm title) theo
  (print (list name firm title)))


(print 'destructuring)

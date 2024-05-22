;; ************************************************
(print '(2.1 Appending Two Lists))

(setq subject1 '(the cow))
(setq subject2 '(the quick brown fox))
(setq predicate1 '(jumped over the moon))
(setq predicate2 '(jumped over the lazy dog))

(defun my-append (x y)
  (if (null x)
      y
      (cons (car x) (my-append (cdr x) y))))

(my-append subject1 predicate1)
(my-append subject2 predicate2)
(my-append subject2 predicate1)

(setq x '(r s t))

(setq y '(u v w))

(setq a (my-append x y))
(setq b (my-append x y))

(my-append '(a a a) '(a a))
(my-append '(a a a) 'nil)
(my-append '(a (b c)) '(b c))
(my-append 'nil '(adds nothing))


;; ************************************************
(print '(2.2 Recursive Functions and Debugging))

(trace my-append)
(my-append '(a b c) '(d e f))

;; Tracing function MY-APPEND.
;; 1. Trace: (MY-APPEND '(A B C) '(D E F))
;; 2. Trace: (MY-APPEND '(B C) '(D E F))
;; 3. Trace: (MY-APPEND '(C) '(D E F))
;; 4. Trace: (MY-APPEND 'NIL '(D E F))
;; 4. Trace: MY-APPEND ==> (D E F)
;; 3. Trace: MY-APPEND ==> (C D E F)
;; 2. Trace: MY-APPEND ==> (B C D E F)
;; 1. Trace: MY-APPEND ==> (A B C D E F)

(untrace my-append)
(untrace)  ;; untrace all

;;(load "filename")  ;; load forms from file


;; ************************************************
(print '(2.3 Reversing a List))

(load "my-reverse.lisp")

(trace bad-reverse my-reverse reverse-aux)

(bad-reverse '(a b c))
(bad-reverse '(a (b c) (d e) f g))
(bad-reverse '((a (b c)) d ((e f g) h)))

(my-reverse '(a b c))
(my-reverse '(a (b c) (d e) f g))
(my-reverse '((a (b c)) d ((e f g) h)))

(untrace)


;; ************************************************
(print '(2.4 Global Variables))

(defvar foo 'bar)
(defvar *count* 0)

(defun count-cons (x y)
  (progn
    (setq *count* (+ 1 *count*))
    (cons x y))
)

(defun count-append (x y)
  (if (null x)
      y
      (count-cons (car x) (count-append (cdr x) y))))

(setq *count* 0)

(count-append '(a b c) '(d e f))

(setq *count* 0)

(count-append '(a b) '(c d e f))

(defun report-append (x y)
  (progn
    (setq *count* 0)
    (count-append x y)
    *count*)
)

(report-append '(a b) '(c d e f))


;; ************************************************
(print '(2.5 EQ - Identical Equality))

(eq 2 (+ 1 1))
(eq 3 3.0)
(eq 2.3 2.3)
(eq 2.3 (+ 1.1 1.2))
(eq (/ 22 7) (/ 22 7))

(setq c '(f 2.3))
(setq d '(f 2.3))

(eq c d)
(eq c c)
(eq d d)
(eq d (car (cons d c)))

(setq x '(r s t))
(setq y '(u v w))
(setq a (append x y))
(setq b (append x y))

(eq a b)
(eq (cdr (cdr a)) (cdr (cdr b)))
(eq (cdr (cdr (cdr a))) (cdr (cdr (cdr b))))

(eq (car a) (car b))
(eq (car (cdr (cdr a))) (car (cdr (cdr b))))

(eq '(a b (c d)) '(a b (c d)))


;; ************************************************
(print '(2.6 EQL - An Intermediate Equality Tester))

(typep 'ztekjo 'symbol)
(typep 'ztekjo 'number)
(typep 'ztekjo 'atom)
(typep '2 'symbol)
(typep '2 'number)
(typep '2 'atom)
(typep -4.34 'fixnum)
(typep -4.34 'single-float)

(eql 'a 'a)
(eql '(a b) (list 'a 'b))
(eql 2 3)
(eql 3 3)

(progn
  (setq *list* (list 'a 'b))
  (setq *big* (list 'c *list*))
  (eql *list* (car (cdr *big*)))
)

(load "my-eql.lisp")

(eql 2.3 (+ 1.1 1.2))
(eql 2 (+ 1 1))
(eql '(a b (c d)) '(a b (c d)))
(eql 3 3.0)

(my-eql 2.3 (+ 1.1 1.2))
(my-eql 2 (+ 1 1))
(my-eql '(a b (c d)) '(a b (c d)))
(my-eql 3 3.0)


;; ************************************************
(print '(2.7 EQUAL - Equalness of Lisp Objects))

(setq x '(r s t))
(setq y '(u v w))

(eq (append x y) (append x y))
(eql (append x y) (append x y))
(equal (append x y) (append x y))

(eq 4.5 (+ 1.2 3.3))
(eql 4.5 (+ 1.2 3.3))
(equal 4.5 (+ 1.2 3.3))

(load "my-equal.lisp")

(trace my-equal)

(my-equal '(f 2.3) '(f 2.3))

(equal 'a 'a)
(equal '(a b) (list 'a 'b))
(equal 2 3)
(equal 2 2)
(progn
  (setq *list* (list 'a 'b))
  (setq *big* (list 'c *list*))
  (equal *list* (car (cdr *big*)))
  )
(equal 2.3 (+ 1.1 1.2)) ;why nil?
(equal '(a b (c d)) '(a b (c d)))
(equal 3 3.0)


;; ************************************************
(print '(2.8 List Creation and Access Functions))

(list* 'a 'b 'c '(d e))
(list* 'a 'b 'c )

(setq clist '(a (b c d) (e f) g))
(cadr clist)
(caadr clist)
(cadadr clist)
(cdaddr clist)

(nth 0 clist)
(nth 3 clist)
(nth 4 clist)

(nthcdr 0 clist)
(nthcdr 1 clist)
(nthcdr 2 clist)
(nthcdr 3 clist)
(nthcdr 4 clist)

(first clist)
(second clist)
(third clist)
(fourth clist)
(fifth clist)
(sixth '(1 2 3 4 5 6))
(seventh (append clist clist))
(eighth (append clist clist))
(ninth (append clist clist '(1)))
(tenth (append clist clist '(1 2)))

(rest clist)

(last clist)
(car (last clist))

(print "E2.8.2 Write two different s-expressions to access symbol c for each of the following lists")

(setq a '(a b c d e))
(nth 2 a)
(caddr a)

(setq b '((a b c) (d e f)))
(first (last (first b)))
(caddar b)

(setq c '((a b) (c d) (e f)))
(caar (nthcdr 1 c))
(caadr c)

(setq d '(a (b c d) e f))
(second (second d))
(cadadr d)


;; ************************************************
(print '(2.9 RPLACA and RPLACD - Surgery on Cons Cells))








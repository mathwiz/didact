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

(print *count*)

(setq *count* 0)

(count-append '(a b) '(c d e f))

(print *count*)

(defun report-append (x y)
  (progn
    (setq *count* 0)
    (count-append x y)
    *count*)
)

(report-append '(a b) '(c d e f))


;; ************************************************
(print '(2.5 EQ Identical Equality))

(eq 2 (+ 1 1))
(eq 2.205 2.205)
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





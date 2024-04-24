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



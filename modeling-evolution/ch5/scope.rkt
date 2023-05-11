#lang racket

(define (fun1 x a b)
  (+ (* a x) b))

; this does't compile
;(define (fun2 x)
;  (+ (* a x) b))

(define (all-args)
  (letrec ((recur (lambda (x a)
                 (cond ((= x 3) #t)
                       (else
                        (println (fun1 x a (* 2 a)))
                        (cond ((= a 3) (recur (add1 x) 1))
                              (else (recur x (add1 a))))
                       )))))
    (recur 1 1)))
(all-args)

(define (test-local-lambda)
  (letrec ((recur (lambda (x a)
                    (define (fun x) (+ (* a x) (* a 2)))
                    (cond ((= x 3) #t)
                          (else
                           (println (fun x))
                           (cond ((= a 3) (recur (add1 x) 1))
                                 (else (recur x (add1 a))))
                           )))))
    (recur 1 1)))
(test-local-lambda)

(define (test-stored-lambda)
  (letrec ((make-funs (lambda (a acc)
                        (cond ((= a 4) (reverse acc))
                              (else (make-funs
                                     (add1 a)
                                     (cons (lambda (x) (+ (* a x) (* a 2))) acc))))))
           (the-funs (make-funs 1 '()))
           (recur (lambda (x funs)
                    (cond ((= x 3) #t)
                          (else
                           (println ((car funs) x))
                           (cond ((> (length funs) 1) (recur x (cdr funs)))
                                 (else (recur (add1 x) the-funs)))
                           )))))
    (recur 1 the-funs)))
(test-stored-lambda)


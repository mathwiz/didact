#lang racket

(define (fun1 x a b)
  (+ (* a x) b))

; this does't compile
;(define (fun2 x)
;  (+ (* a x) b))

(println "Using externally defined function that passes parameters a and b.")
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

(println "Using locally created lambdas. Note how context has name a.")
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

(println "Using stored lambdas that capture local value b. Note how context has name a with wrong value.")
(define (test-stored-lambda)
  (letrec ((make-funs (lambda (a b acc)
                        (cond ((= a 4) (reverse acc))
                              (else (make-funs
                                     (add1 a)
                                     (* 2 (add1 a))
                                     (cons (lambda (x) (+ (* a x) b)) acc))))))
           (the-funs (make-funs 1 2 '()))
           (recur (lambda (x a funs)
                    (cond ((= x 3) #t)
                          (else
                           (println ((car funs) x))
                           (cond ((> a 0) (recur x (sub1 a) (cdr funs)))
                                 (else (recur (add1 x) 2 the-funs)))
                           )))))
    (recur 1 2 the-funs)))
(test-stored-lambda)


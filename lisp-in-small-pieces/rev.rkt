#lang racket

(define (rev l)
  (define nil '())
  (define (reverse l r)
    (if (pair? l) (reverse (cdr l) (cons (car l) r)) r) )
  (reverse l nil) )
(define *small* 1)
(define *big* 1000)
	
(define (new-game)
	(set! *small* 1)
	(set! *big* 1000))

(define (guess-my-number)
  (quotient (+ *small* *big*) 2))

(define (smaller)
  (set! *big* (- (guess-my-number) 1))
  (guess-my-number))

(define (bigger)
  (set! *small* (+ (guess-my-number) 1))
  (guess-my-number))

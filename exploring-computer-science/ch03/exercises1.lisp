(print "Section 3.2 Numerical Functions")

(print (list 
(- 4)
(/ 4)
(- 4 -2 -3)
(/ 24 6 2)
(expt 4 3)
;(remainder 4 3)
(rem 43 29)
;(sqrt 9 16)
(+)
(*)
;(/)
(+ 3)
))


(print "Section 3.6 Variables")

(defvar foo 7.32)
(print foo)
(setq foo (* foo 2))
(print foo)


(print "Section 3.10 Programming Style")
(print "Assuming no knowledge beyond this point in book.")

(print "3.13 A function that takes 3 numbers and returns their average")

(defun avg3 (a b c)
  (/ (+ a b c) 3.0))

(print (avg3 10 20 40))

(print "3.14 A function that takes 5 numbers and returns the average of the middle 3")

(defun avg-mid3of5 (a b c d e)
  (/ 
   (- (+ a b c d e)
      (min a b c d e)
      (max a b c d e)
      )
   3.0))

(print (avg-mid3of5 10 20 40 99 4))

(print "3.15 A function that converts Fahrenheit to Celsius")

(defun tempFtoC (f)
  (* (- f 32.0) (/ 5 9)))

(print (tempFtoC 100))

(print "3.16 A function that converts Celsius to Fahrenheit")

(defun tempCtoF (c)
  (+ (* c (/ 9 5)) 32.0))

(print (tempCtoF 37))

(print "3.17 A function that gives the next year of the dragon given a previous dragon year and the present year")

(defun next-yod (yod now)
  (+ (* c (/ 9 5)) 32.0))

(print (next-yod 1964 2023))








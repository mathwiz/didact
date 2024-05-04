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
  (if (= (rem (- now yod) 12) 0)
      now
      (+ (- 12 (rem (- now yod) 12)) now)))

(print (next-yod 1964 2012))
(print (next-yod 1964 2023))
(print (next-yod 1964 2028))

(print "3.18 A function to compute annual spending from daily, weekly, and monthly spending")

(defun annual-spending (daily weekly monthly)
  (+
      (* daily 365)
      (* weekly 52)
      (* monthly 12)))

(print (annual-spending 25 300 1400))

(print "3.19 A function to calculate the number of years to live on a given quantity of money")

(defun years-on-spending (assets)
  (+ 0.0
     (/ assets (annual-spending 25 500 1400))))

(print (years-on-spending (* (expt 10.0 6) 1.5)))

(print "3.20 A function to convert Thai baht to US dollars")

(defun baht-to-dollars (baht)
  (/ (* baht 4)
     100.0))

(print (baht-to-dollars (* (expt 10.0 6) 1.5)))













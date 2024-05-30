(print "Chapter 4. Lists: The Basic Data Structure")

(print "4.1 Lists in Scheme")

((a b c) (1 2 3))

(sqrt 4)


(print "4.2 Stopping Evalutation with quote")

(quote (+ 2 3))
'(+ 2 3)

'('(a b c) '(1 2 3))
'((a b c) (1 2 3))

'()

#'sqrt
(apply #'sqrt '(7))

(setq num 3.3)
num 
(quote num)


(print "4.3 Special Forms")

(print "Excercises 4.3.1")

(print "4.1 Why are define, let, and let* implemented as special forms insteat of regular functions?")

(print "4.2 Which arguments to define and let are evaluated and which arguments are not evalutated?")

(print "4.3 What do the following expressions evaluate to? Some may produce errors.")

''(a b c)
; error ('a 'b 'c)
'('a 'b 'c)
; error (quote a (1 2))
(quote '(1 2))
()
; error ((+ 1 2))


(print "4.4 Using Lists as Data Structures")

(print "Exercises 4.4.1")

'(rock
  (Rolling_Stones
   (Black_and_Blue
    Its_Only_Rock_and_Roll))
  jazz
  (Pat_Metheney
   (First_Circle)
   Andy_Narell
   (The_Hammer)))


(print "4.4 What other ways can you think of to organize a collection of CDs?")

(print "4.5 What advantages and/or disadvantages does your data structure have compared to this one?")

(print "4.6 Design a data structure that you could use to maintain information on students: name, student ID number, year in school, address, grades, grade point average, etc.")

'(class
  (Emma_Woodhouse
   (Hartfield
    Highbury)
   1
   First
   (A B+ C A-))
  (Jane_Fairfax
   (Campbell's
    London)
   2
   Second
   (A A+ B+ A)))


(print "4.5 Taking Lists Apart")





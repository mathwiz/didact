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

(length '(1 2 3 (4 5) 6))
(first '((1 a) 2 3 4 6))
(rest '(1 2 3 (4 5) 6))
(last '(1 2 3 (4 5) 6))
(nth 3 '(1 2 3 (4 5) 6))

(defun last-elem (lst)
  (nth (1- (length lst)) lst))

(last-elem '(1 2 3 (4 5) 6))

(print "4.5.1 Example: Extracting random elements from a data structure")

(setq retort
'(
(i am sorry but we are closed now)
(talk to the person at the end of the hall)
(you need form 1044-A and not 1044-B)
(we cannot take personal checks)
(i am sorry we need exact change)
(oh you only had to fill out this one form not those 20 others)
))

(defun get-random-element (lst)
  (nth (random (length lst)) lst))

(get-random-element retort)

(print "4.5.2 Exercises")

(print "4.7 What do the following expressions return?")

(car '())
(cdr '())






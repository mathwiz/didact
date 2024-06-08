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
(third (subseq '((4 5) 1/3 67.89 (78) value) 1))
(rest (subseq '(how (strange) (((this)) may) seem) 1 4))
(length '('a '(1 2)))
(length '((3 elements here)))
(length '((yet ((another)) strange (list))))
(car '((yet ((another)) strange (list))))
(cdr '((yet ((another)) strange (list))))
(car (car '((yet ((another)) strange (list)))))
(car (cdr (cdr (car '((yet ((another)) strange (list)))))))

(print "4.8 Assume the function extract will be called with a list of lists of atoms. Create versions so that it returns a) the first list of atoms, b) the first atom.")

(let 
    ((list-of-lists '((a b c) (1 2 3))))
(print ((lambda (lol) (first lol)) list-of-lists))
(print ((lambda (lol) (first (first lol))) list-of-lists))
'nil
)

(print "4.9 Write an expression that returns the third element of a list.")

(let ((a-list (list 1 2 3 4 5 6)))
  (third a-list)
)

(print "4.10 Write an expression that returns element number (+ value 2) of the list a-list.")

(let ((a-list (list 1 2 3 4 5 6))
      (value 3))
  (car (subseq a-list (+ value 1) (+ value 2)))
)

(print "4.11 Write an expression that returns the list of CDs from the second jazz artist.")

(let 
((struct 
  '(rock
  (Rolling_Stones
   (Black_and_Blue
    Its_Only_Rock_and_Roll))
  jazz
  (Pat_Metheney
   (First_Circle)
   Andy_Narell
   (The_Hammer)))
))
(first (subseq (fourth struct) 2 3))
)

(print "4.12 Write your own version of list-ref using the other list functions.")

(let ((a-list (list 1 2 3 4 5 6)))
(flet ((list-ref (xs zero-index)
         (nth zero-index xs))
)
(list-ref a-list 4)
))

(print "4.13 Write a function but-last that takes two arguments, a-list and num, and returns a list of all but the last num elements of a-list.")

(let ((a-list (list 1 2 3 4 5 6)))
(flet ((but-last (xs num)
         (subseq xs 0 (- (length xs) num)))
)
(but-last a-list 4)
))

(print "4.14 Write a function start that takes two arguments, a-list and num, and returns the first num elements from a-list.")

(let ((a-list (list 1 2 3 4 5 6)))
(flet ((start (xs num)
         (subseq xs 0 num))
)
(start a-list 4)
))

(print "4.15 Write a function end that takes two arguments, a-list and num, and returns the last num elements from a-list.")

(let ((a-list (list 1 2 3 4 5 6)))
(flet ((end (xs num)
         (subseq xs (- (length xs) num)))
)
(end a-list 4)
))

(print "4.16 Fix the function month so that it returns the month corresponding to month-num.")

(defun month (month-num)
  (nth (1- month-num)
       '(January
         February
         March
         April
         May
         June
         July
         August
         September
         October
         November
         December)))

(print (month 4))

(print "4.17 The function replace-element takes a-list, position, and element, an atom that will replace the element at position in a-list.")

(defun replace-element (a-list position element)
  (append
   (subseq a-list 0 position)
   (cons element (subseq a-list (1+ position)))
))

(print (replace-element '(this list is very mundane i think) 4 'exciting))

(print "4.18 Fill in the functions and arguments so that the output shown is produced. There may be zero or more arguments.")

(let ((a-list '(a list of sorts)))
(print a-list)
(print '(a list))
(print (subseq a-list 0 2))
(print '(list of))
(print (subseq a-list 1 3))
(print '(of sorts))
(print (subseq a-list 2 4))
(print 'list)
(print (nth 1 a-list))
)


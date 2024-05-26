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









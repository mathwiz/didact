(print "4.6 Combining carS and cdrS")

(cddadr '(1 ((a b) (c d) (e f)) 2 3))


(print "4.7 Creating Lists")

(list 'a 'b 'c)

(append nil (list 'a 'b 'c))

(defun add-to-end (it lst)
  (append lst (list it)))


(print "4.7.1 Exercises")

(print "4.19 What do the following expressions evaluate to? Some of them may produce errors.")

(cons 3 '(4))
(cons '(3) '(4))
(list 3 '(4))
(list '(3) '(4))
(list 3 4)
(list '(3) 4)
(append 3 '(4))
(append '(3) '(4))

(print "4.20 Assume that the following defines have been made:")

(defvar numbers '(2 4 6))
(defvar letters '(q e d))
(defvar deep-list '(((13))))

(print "Using only these three variables and the functions cons, list, and append, write expressions that will return the following lists:
(2 4 6 q e d ((13)))
((2 4 6) (q e d) ((13)))
(2 4 6 (q e d) ((13)))
((2 4 6) (q e d) (((13))))
((2 4 6) q e d ((13)))
")

;; (2 4 6 q e d ((13)))
(append numbers letters deep-list)

;; ((2 4 6) (q e d) ((13)))
(append (list numbers) (list letters) deep-list)

;; (2 4 6 (q e d) ((13)))
(append numbers (list letters) deep-list)

;; ((2 4 6) (q e d) (((13))))
(append (list numbers) (list letters) (list deep-list))

;; ((2 4 6) q e d ((13)))
(append (list numbers) letters deep-list)


(print "4.21 Fill in the blanks with functions and arguments so that the output shown would be produced.")

;; (alas all lists are not created equal)
(append '(alas all lists are) '(not created equal))

;; ((alas all lists are) not created equal)
(append (list '(alas all lists are)) '(not created equal))

;; (lists (not created equal))
(list (third '(alas all lists are)) '(not created equal))

;; ((lists) (not created equal))
(list (subseq '(alas all lists are) 2 3) '(not created equal))


(print "4.22 Write a function that takes a list a-list and returns a list of only the first element of a-list. Note: this is not the same as first or car. For example, given the list (an apple a day), your function should return (an).")




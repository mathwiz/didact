(print "P2.1 Write a function ALL-LENGTH of one argument that counts the number of atoms that occur in a list at all levels.")

(defun all-length (xs)
  (if (null xs)
      0
      (+ (all-length (cdr xs)) 1)))

(eq (all-length '(a b c)) 3)
(eq (all-length '(a (b c) d (e f))) 6)
(eq (all-length '((a 1.2) (b (c d) 3.14) (e))) 7)
(eq (all-length '(nil nil (nil nil) nil)) 5)




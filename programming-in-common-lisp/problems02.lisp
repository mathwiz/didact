(print "P2.1 Write a function ALL-LENGTH of one argument that counts the number of atoms that occur in a list at all levels.")

(defun all-length (xs)
  (if (atom xs)
      1
      (+ (all-length (cdr xs)) 1)))

(eq (all-length '(a b c)) 3)
(eq (all-length '(a (b c) d (e f))) 6)
(eq (all-length '((a 1.2) (b (c d) 3.14) (e))) 7)
(eq (all-length '(nil nil (nil nil) nil)) 5)


(print "P2.2 Write a function ALL-REVERSE of one argument that reverses a list at all levels and thus results in the following transformations.")

(defun all-reverse (xs)
  (if (atom xs)
      xs
      (all-reverse-aux (reverse xs))))

(defun all-reverse-aux (xs)
  (if (null (cdr xs))
      (car xs)
      (cons (all-reverse (car xs))
            (all-reverse (cdr xs)))))

(equal (all-reverse '(a b c d)) '(d c b a))
(equal (all-reverse '(a (b c) (d e))) '((e d) (c b) a))
(equal (all-reverse '(a (b c) d (e f g) h)) '(h (g f e) d (c b) a))


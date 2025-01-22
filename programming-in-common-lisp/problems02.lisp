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
  (all-reverse-aux xs 'nil))

(defun all-reverse-aux (rem sofar)
  (if (null rem)
      sofar
      (all-reverse-aux (cdr rem)
                   (cons (if (atom (car rem))
                             (car rem)
                             (all-reverse-aux (car rem) 'nil))
                         sofar))))

(equal (all-reverse '(a b c d)) '(d c b a))
(equal (all-reverse '(a (b c) (d e))) '((e d) (c b) a))
(equal (all-reverse '(a (b c) d (e f g) h)) '(h (g f e) d (c b) a))


(print "P2.3 Write COUNT-REVERSE-1 and COUNT-REVERSE-2 paralleling the two versions of REVERSE that we studied in section 2.3 using COUNT-APPEND and COUNT-CONS of section 2.4. Write a function COMPARE-REVERSES that takes a list compares how many more cons cells are used by COUNT-REVERSE-1 than by COUNT-REVERSE-2 to reverse the list. Run COMPARE-REVERSES on a few lists of different lengths. For example,
(COMPARE-REVERSES '(a b c))
(COMPARE-REVERSES '(a b c d e f))
You may have to write auxilliary functions to achieve this. Do no use any global variables other than *COUNT*.")

(defvar *COUNT* 0)

(defun count-cons (x y)
  (progn (setq *COUNT* (1+ *COUNT*))
         (cons x y)))

(defun count-append (x y)
  (if (null x)
      y
      (count-cons (car x)
                  (count-append (cdr x) y))))

(count-append '(a b c) '(d e f))

(defun count-reverse-1 (lst)
  (if (null lst)
      'nil
      (count-append (count-reverse-1 (cdr lst)) (count-cons (car lst) 'nil))))

(defun count-reverse-aux (rem sofar)
  (if (null rem)
      sofar
      (count-reverse-aux (cdr rem) (count-cons (car rem) sofar))))

(defun count-reverse-2 (lst)
  (count-reverse-aux lst 'nil))

(defun compare-reverses (lst)
  (progn
    (print "Counting reverses on")
    (print lst)
    (setq *COUNT* 0)
    (count-reverse-1 lst)
    (print *COUNT*)
    (setq *COUNT* 0)
    (count-reverse-2 lst)
    (print *COUNT*)
    'nil
))

(setq *COUNT* 0)
(COMPARE-REVERSES '(a b c))
(COMPARE-REVERSES '(a b c d e f))


(print "P2.4 Write a function EQUAL-SYMBOLS of two arguments that says any two numbers are equal, a symbol is equal to itself, and a list is equal if all their elements are recursively EQUAL-SYMBOLS. Thus, the function has the following results:
(EQUAL-SYMBOLS 2.2 3)
T
(EQUAL-SYMBOLS 2.2 'A)
nil
(EQUAL-SYMBOLS '(B 4 (C D (2.2)) E) '(B 2 (C D (0)) E)))
T
(EQUAL-SYMBOLS '(B 4 (C D (2.2)) E) '(B 2 (3 D (0)) E)))
nil
")



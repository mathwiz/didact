(defvar my-hand '((3 hearts)
                  (5 clubs)
                  (2 diamonds)
                  (4 diamonds)
                  (ace spades)))

(defvar colors '((clubs black)
                 (diamonds red)
                 (hearts red)
                 (spades black)))

(defvar all-ranks '(2 3 4 5 6 7 8 9 10 jack queen king ace))

;;; Card Rank

(defun rank (card)
  (first card))

(defun suit (card)
  (second card))

(defun color-of (card)
  (second (assoc (suit card) colors)))

(defun first-red (hand)
  (find-if (lambda (x) (equal (color-of x) 'red)) hand))

(defun first-black (hand)
  (find-if (lambda (x) (equal (color-of x) 'black)) hand))

(defun black-cards (hand)
  (remove-if-not (lambda (x) (equal (color-of x) 'black)) hand))

(defun red-cards (hand)
  (remove-if-not (lambda (x) (equal (color-of x) 'red)) hand))

(defun what-ranks (thesuit hand)
  (mapcar
   #'first
   (remove-if-not (lambda (x) (equal (suit x) thesuit)) hand)))

(defun higher-rank-p (card1 card2)
  (let ((lower-card
         (lambda (x y) (member (rank y) (member (rank x)
                                                all-ranks)))))
    (if (funcall lower-card card1 card2)
        card2
        card1)))

(defun high-card (hand)
  (reduce #'higher-rank-p hand))

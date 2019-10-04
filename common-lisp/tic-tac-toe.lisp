;; Board representation
;; 1 | 2 | 3
;; 4 | 5 | 6
;; 7 | 8 | 9
;;
;; X = 10, O = 1, empty = 0
;;


(defun make-board ()
  (list 'board 0 0 0 0 0 0 0 0 0))

(defun convert-to-letter (val)
  (cond ((equal val 10) "X")
	((equal val 1) "O")
	(t " ")))

(defun print-row (x y z)
  (format t "~& ~A | ~A | ~A"
	  (convert-to-letter x)
	  (convert-to-letter y)
	  (convert-to-letter z)))

(defun print-board (board)
  (format t "~%")
  (print-row
   (nth 1 board) (nth 2 board) (nth 3 board))
  (format t "~&-----------")
  (print-row
   (nth 4 board) (nth 5 board) (nth 6 board))
  (format t "~&-----------")
  (print-row
   (nth 7 board) (nth 8 board) (nth 9 board))
  (format t "~&~&")
  )

(defun make-move (player pos board)
  (setf (nth pos board) player)
  board)

(defvar b)
(setf b (make-board))

(defvar *computer*)
(defvar *opponent*)
(setf *computer* 10)
(setf *opponent* 1)

(defvar *triplets*)
(setf *triplets*
      '((1 2 3) (4 5 6) (7 8 9)
	(1 4 7) (2 5 8) (3 6 9)
	(1 5 9) (3 5 7)))

(defvar *corners*)
(setf *corners*
      '(1 3 7 9))

(defvar *sides*)
(setf *sides*
      '(2 4 6 8))

(defun sum-triplet (board triplet)
  (+ (nth (first triplet) board)
     (nth (second triplet) board)
     (nth (third triplet) board)
     ))

(defun compute-sums (board)
  (mapcar #'(lambda (triplet)
	      (sum-triplet board triplet))
	  *triplets*))

(defun winner-p (board)
  (let ((sums (compute-sums board)))
    (or (member (* 3 *computer*) sums)
	(member (* 3 *opponent*) sums))))

(defun play-one-game ()
  (if (y-or-n-p "Would you like to go first? ")
      (opponent-move (make-board))
      (computer-move (make-board))))

(defun opponent-move (board)
  (let* ((pos (read-a-legal-move board))
	 (new-board (make-move *opponent* pos board)))
    (print-board new-board)
    (cond ((winner-p new-board) (format t "~&You win!"))
	  ((board-full-p new-board) (format t "~&Tie game!"))
	  (t (computer-move new-board)))))

(defun read-a-legal-move (board)
  (format t "~&Your move: ")
  (let ((pos (read)))
    (cond ((not (and (integerp pos) (<= 1 pos 9)))
	   (format t "~&Invalid input.")
	   (read-a-legal-move board))
	  ((not (zerop (nth pos board)))
	   (format t "~&That space is already occupied.")
	   (read-a-legal-move board))
	  (t pos))))

(defun board-full-p (board)
  (not (member 0 board)))

(defun computer-move (board)
  (let* ((best-move (choose-best-move board))
	 (pos (first best-move))
	 (strategy (second best-move))
	 (new-board (make-move *computer* pos board)))
    (format t "~&My move: ~S" pos)
    (format t "~&My strategy: ~A~%" strategy)
    (print-board new-board)
    (cond ((winner-p new-board) (format t "~&I win!"))
	  ((board-full-p new-board) (format t "~&Tie game!"))
	  (t (opponent-move new-board)))))

(defun choose-best-move (board)
  (or (make-three-in-a-row board)
      (block-opponent-win board)
      (block-squeeze-play board)
      (random-move-strategy board)))

(defun random-move-strategy (board)
  (list (pick-random-empty-position board)
	"random move"))

(defun pick-random-empty-position (board)
  (let ((pos (+ 1 (random 9))))
    (if (zerop (nth pos board))
	pos
	(pick-random-empty-position board))))

(defun make-three-in-a-row (board)
  (let ((pos (win-or-block board (* 2 *computer*))))
    (and pos (list pos "make three in a row"))))

(defun block-opponent-win (board)
  (let ((pos (win-or-block board (* 2 *opponent*))))
    (and pos (list pos "block opponent"))))

(defun block-squeeze-play (board)
  (let ((pos (find-squeeze-play board *computer* *opponent*)))
    (and pos (list pos "block squeeze play"))))

(defun win-or-block (board target-sum)
  (let ((triplet (find-if #'(lambda (trip)
			      (equal (sum-triplet board trip)
				     target-sum))
			  *triplets*)))
    (when triplet (find-empty-position board triplet))))

(defun find-squeeze-play (board player player-to-block)
  (let* ((triplet (strategic-diagonal
				 board
				 (+ (* 2 player-to-block)
				    player))))
    (when (if (and triplet (= (second triplet) 5)) triplet nil)
      (find-empty-position
       board
       *sides*))))

(defun diagonal-p (trip)
  (and (or (and (= (first trip) 1) (= (third trip) 9))
	   (and (= (first trip) 3) (= (third trip) 7)))
       (= (second trip) 5)))

(defun strategic-diagonal (board target-sum)
  (find-if #'(lambda (trip)
	       (and (equal (sum-triplet board trip)
			   target-sum)
		    (diagonal-p trip)))
	   *triplets*))

(defun find-empty-position (board squares)
  (find-if #'(lambda (pos)
	       (zerop (nth pos board)))
	   squares))

(defun block-squeeze-play (board)
  (let ((pos (block-squeezes board 2)))
    (and (pos (list pos "block squeeze play")))))


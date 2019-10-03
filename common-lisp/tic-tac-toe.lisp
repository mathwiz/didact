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
   (nth 4 board) (nth 5 board) (nth 5 board))
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
(setf *computer* 1)
(setf *opponent* 0)

(defvar *triplets*)
(setf *triplets*
      '((1 2 3) (4 5 6) (7 8 9)
	(1 4 7) (2 5 8) (3 6 9)
	(1 5 9) (3 5 7)))

(defun sum-triple (t)
  0)
;; Board representation
;; 1 | 2 | 3
;; 4 | 5 | 6
;; 7 | 8 | 9
;;
;; X = 1, O = 0, empty = -1
;;


(defun make-board ()
  (list 'board -1 -1 -1 -1 -1 -1 -1 -1 -1))

(defun convert-to-letter (val)
  (cond ((equal val 1) "X")
	((equal val 0) "O")
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


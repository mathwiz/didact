(defvar *cache* (make-hash-table))
(defvar *world* (make-hash-table))

(defun retrieve (key)
  (multiple-value-bind (x y) (gethash key *cache*)
    (if y
        (values x y)
        (cdr (assoc key *world*)))))


(defsetf retrieve (key) (val)
  `(setf (gethash ,key *cache*) ,val))


;; example
(setf (gethash 'a *world*) 2)
(setf (gethash 'b *world*) 16)
(setf (gethash 'c *world*) 50)
(setf (gethash 'd *world*) 20)
(setf (gethash 'f *world*) 12)

(write (gethash 'f *world*))
(write-line "")
(write (retrieve 'c))
(write-line "")

(setf (retrieve 'n) 77)
(write (retrieve 'n))

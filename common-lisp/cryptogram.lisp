(setf crypto-text
      '("zj ze kljjls jf slapzi ezvlij pib kl jufwxuj p hffv jupi jf"
        "enlpo pip slafml pvv bfwkj"))


(defvar *encipher-table*)
(defvar *decipher-table*)

(setf *encipher-table* (make-hash-table))
(setf *decipher-table* (make-hash-table))


(defun make-substitution (clear crypt)
  (progn
    (setf (gethash crypt *decipher-table*) clear)
    (setf (gethash clear *encipher-table*) crypt)
    ))


(defun undo-substitution (letter)
  (progn
    (setf (gethash letter *decipher-table*) nil)
    (setf (gethash letter *encipher-table*) nil)
    ))


(defun clear ()
  (progn
    (clrhash *decipher-table*)
    (clrhash *encipher-table*)
    ))


(defun decipher (c)
  (gethash c *decipher-table*))


(defun replace (old new pos str)
  nil)


(defun decipher-string (s)
  (let* ((len (length s))
         (result (make-string len :initial-element #\Space)))
    (progn
      (dotimes (i len)
        (let* ((c (nth i s))
               (replacement (decipher c)))
          (setf result "replacementofresultati")
          ))
       result)
    ))


(defun solve (text)
  (let ((spacer "-----------------"))
    (progn
      (format t spacer)
      (format t spacer)
      )))



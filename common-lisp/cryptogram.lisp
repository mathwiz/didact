(setf crypto-text
      '("zj ze kljjls jf slapzi ezvlij pib kl jufwxuj p hffv jupi jf"
        "enlpo pip slafml pvv bfwkj"))


(defvar *encipher-table*)
(defvar *decipher-table*)

(setf *encipher-table* (make-hash-table))
(setf *decipher-table* (make-hash-table))


(defun sym-to-char (s)
  s)


(defun char-to-sym (c)
  c)


(defun make-substitution (clear crypt)
  (let ((x nil))
    (progn
      (setf (gethash crypt *decipher-table*) clear)
      (setf (gethash clear *encipher-table*) crypt))
  ))


(defun undo-substitution (letter)
  (let ((x nil))
    (progn
      (setf (gethash letter *decipher-table*) nil)
      (setf (gethash letter *encipher-table*) nil))
  ))


(defun clear ()
  (progn
    (clrhash *decipher-table*)
    (clrhash *encipher-table*)
    ))


(defun decipher (c)
  (gethash c *decipher-table*))


(defun replace-char (new old pos str)
  (substitute new old str :start pos :end (1+ pos)))


(defun decipher-string (s)
  (let* ((len (length s))
         (result (make-string len :initial-element #\space)))
    (progn
      (dotimes (i len)
        (let* ((c (char s i))
               (replacement (decipher c)))
          (if replacement
              (setf result (replace-char replacement #\space i result)))
          ))
       result)
    ))


(defun show-line (s)
  (format t "~S~%~S" s (decipher-string s)))


(defun show-text (cryptogram)
  (dolist (x cryptogram nil)
    (show-line x)))


(defun get-first-char (x)
  (char-downcase (char (format nil "~A" x) 0)))


(defun solve (text)
  (let ((spacer "-----------------"))
    (progn
      (format t spacer)
      (format t spacer)
      text
      )))


;; Testing
(defvar test-crypt)
(defvar test-result)

(setf test-crypt "zyxabc")
(setf test-result "      ")

(make-substitution #\a #\z)
(make-substitution #\b #\y)
(make-substitution #\c #\x)
(make-substitution #\d #\w)

(princ (decipher #\z))

(print test-crypt)
(print test-result)
(dotimes (i (length test-crypt))
  (let* ((c (char test-crypt i))
         (r (decipher c)))
    (progn
      (print c)
      (print r)
      (if r
          (print (setf test-result (replace-char r #\space i test-result))))
      )
    ))

(print test-result)

(print (decipher-string test-crypt))


(setf crypto-text
      '("zj ze kljjls jf slapzi ezvlij pib kl jufwxuj p hffv jupi jf"
        "enlpo pib slafml pvv bfwkj"))


(defvar *encipher-table*)
(defvar *decipher-table*)
(defvar *spacer*)

(setf *encipher-table* (make-hash-table))
(setf *decipher-table* (make-hash-table))
(setf *spacer* "------------------------")


(defun make-substitution (crypt clear)
  (progn
    (setf (gethash crypt *decipher-table*) clear)
    (setf (gethash clear *encipher-table*) crypt)))


(defun undo-substitution (letter)
  (progn
    (setf (gethash letter *decipher-table*) nil)
    (setf (gethash letter *encipher-table*) nil)
    letter))


(defun clear ()
  (progn
    (clrhash *decipher-table*)
    (clrhash *encipher-table*)))


(defun decipher (c)
  (gethash c *decipher-table*))


(defun enciphered (c)
  (gethash c *encipher-table*))


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
  (format t "~%~A~%~A" s (decipher-string s)))


(defun show-text (cryptogram)
  (dolist (x cryptogram nil)
    (show-line x)))


(defun get-first-char (x)
  (char-downcase (char (format nil "~A" x) 0)))


(defun read-letter ()
  (let ((in (read)))
    (cond ((or (equal 'undo in) (equal 'end in)) in)
          (t (get-first-char in)))))


(defun sub-letter (c)
  (let ((d (decipher c)))
    (if d
      (format t "'~A' has already been deciphered as '~A'!" c d)
      (progn
        (format t "What does '~A' decipher to? " c)
        (try-substitution (read-letter) c)
        ))
    ))


(defun try-substitution (clear crypt)
  (let ((existing (enciphered clear)))
    (if existing
        (format t "But '~A' already deciphers to '~A'!" existing clear)
      (make-substitution crypt clear))
    ))


(defun undo-letter ()
  (progn
    (format t "Undo which letter? ")
    (let* ((c (read-letter))
           (d (decipher c)))
      (if d
          (undo-substitution c)
        (progn
          (format t "'~A' does not have a decipherment." c)
          nil)))))


(defun solve (text)
  (progn
    (format t "~%~A" *spacer*)
    (show-text text)
    (format t "~%~A" *spacer*)
    (progn
      (format t "~%Substitute which letter? ")
      (let ((in (read-letter)))
        (cond ((characterp in)
               (sub-letter in))
              ((equal in 'undo)
               (undo-letter))
              ((equal in 'end)
               t)
              (t (format t "Invalid input")))
        (if (equal in 'end)
          t
          (solve text))
      ))
  ))



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
;      (print c)
;      (print r)
      (if r
          (print (setf test-result (replace-char r #\space i test-result))))
      )
    ))

(print (decipher-string test-crypt))

(print "deciphered")
(show-text crypto-text)

;; Run
(clear)
(make-substitution #\p #\a)
(make-substitution #\z #\i)


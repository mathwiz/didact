(defun print-tag (name alst closingp)
  (princ #\<)
  (when closingp
    (princ #\/))
  (princ (string-downcase name))
  (mapc (lambda (att)
          (format t " ~a=\"~a\""
                  (string-downcase (car att))
                  (cdr att)))
        alst)
  (princ #\>)
)


;; Examples
(print-tag 'mytag '((color . blue) (height . 9)) nil) (fresh-line)
(print-tag 'p nil T) (fresh-line)


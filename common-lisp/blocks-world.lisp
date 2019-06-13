(defvar DATABASE
  '((b1 shape brick)
    (b1 color green)
    (b1 size large)
    (b1 left-of b4)
    (b1 left-of b5)
    (b1 left-of b6)
    (b2 shape brick)
    (b2 color red)
    (b2 size small)
    (b2 supports b1)
    (b2 left-of b3)
    (b2 left-of b4)
    (b2 left-of b5)
    (b2 left-of b6)
    (b3 shape brick)
    (b3 color red)
    (b3 size small)
    (b3 supports b1)
    (b3 left-of b4)
    (b3 left-of b5)
    (b3 left-of b6)
    (b4 shape pyramid)
    (b4 color blue)
    (b4 size small)
    (b4 left-of b6)
    (b5 shape cube)
    (b5 color green)
    (b5 size large)
    (b5 left-of b6)
    (b6 shape brick)
    (b6 color purple)
    (b6 size large)
))

(defun match-element (a b)
  (or (equal b '?) (equal a b)))

(defun match-triple (a b)
  "More complicated than necessary to illustrate techniques."
  (every (lambda (x) x) (mapcar (lambda (x) (match-element (first x) (second x))) (mapcar #'list a b))))

(defun fetch (pat)
  (remove-if-not (lambda (x) (match-triple x pat)) DATABASE))






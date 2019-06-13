(defvar DATABASE)
(setf DATABASE
      '((b1 shape brick)
	(b1 color green)
	(b1 size small)
	(b1 supported-by b2)
	(b1 supported-by b3)
	(b2 shape brick)
	(b2 color red)
	(b2 size small)
	(b2 supports b1)
	(b2 left-of b3)
	(b3 shape brick)
	(b3 color red)
	(b3 size small)
	(b3 supports b1)
	(b3 right-of b2)
	(b4 shape pyramid)
	(b4 color blue)
	(b4 size large)
	(b4 supported-by b5)
	(b5 shape cube)
	(b5 color green)
	(b5 size large)
	(b5 supports b4)
	(b6 shape brick)
	(b6 color purple)
	(b6 size large)
	))

(defun match-element (a b)
  (or (equal b '?) (equal a b)))

(defun match-triple (a b)
  "More complicated than necessary to illustrate techniques."
  (every #'(lambda (x) x) (mapcar (lambda (x) (match-element (first x) (second x))) (mapcar #'list a b))))

(defun fetch (pat)
  (remove-if-not #'(lambda (x) (match-triple x pat)) DATABASE))

(fetch '(b4 shape ?))
(fetch '(? shape brick))
(list (fetch '(b3 ? b2)) (fetch '(b2 ? b3)))
(fetch '(b4 ? ?))
(fetch '(? color ?))

(defun color-pat (x)
  (list x 'color '?))

(defun shape-pat (x)
  (list x 'shape '?))

(defun supporters (x)
  (mapcar #'first (fetch (list '? 'supports x))))

(defun is-cube (x)
  (not (not (fetch (list x 'shape 'cube)))))

(defun supp-cube (b)
  (remove-if-not #'is-cube (supporters b)))

(defun desc2 (b)
  (mapcar #'(lambda (x) (list (second x) (third x))) (fetch (list b '?
  '?))))

(defun description (b)
  (reduce #'append (desc2 b)))



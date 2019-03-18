(defvar NOTE-TABLE)

(setf NOTE-TABLE '((C 1) 
                   (C-SHARP 2) 
                   (D 3) 
                   (D-SHARP 4) 
                   (E 5) 
                   (F 6) 
                   (F-SHARP 7) 
                   (G 8) 
                   (G-SHARP 9) 
                   (A 10) 
                   (A-SHARP 11) 
                   (B 12)))


(defun numbers (notes)
  (mapcar (lambda (x) (cadr (assoc x note-table))) notes))


(defun notes (nums)
  (let ((rev-note-table (mapcar #'reverse note-table)))
    (mapcar (lambda (x) (cadr (assoc x rev-note-table))) nums)))


(defun raise (amount nums)
  (mapcar (lambda (x) (+ x amount)) nums))


(defun normalize (nums)
  (let ((normer (lambda (x)
                  (cond ((< x 1) (+ x 12))
                        ((> x 12) (mod x 12))
                        (t x)))))
    (mapcar normer nums)))


(defun transpose (steps notes)
  (notes (normalize (raise steps (numbers notes)))))

(defmacro let1 (var val &body body) 
  `(let ((,var ,val)) ,@body))


(defmacro split (val yes no) 
  (let1 g (gensym) 
        `(let1 ,g ,val (if ,g (let ((head (car ,g)) 
                                    (tail (cdr ,g))) ,yes) ,no))))


(defun pairs (lst) 
  (labels ((f (lst acc) 
             (split lst (if tail (f (cdr tail) 
                                    (cons (cons head (car tail)) acc)) 
                            (reverse acc)) 
                    (reverse acc)))) 
    (f lst nil)))


(defmacro recurse (vars &body body) 
  (let1 p (pairs vars) 
        `(labels ((self ,(mapcar #'car p) ,@body)) 
           (self ,@(mapcar #'cdr p)))))


(defun my-length (lst) 
  (recurse (lst lst acc 0) 
           (split lst (self tail (1+ acc)) acc)))


(defun my-length2 (lst) 
  (reduce (lambda (x i) 
            (1+ x)) lst 
            :initial-value 0))



;; testing
(princ (pairs '(1 2 3 4 5)))

(fresh-line)

(split '(1 2 3) 
       (format t "list with head == ~a and tail == ~a~%" head tail) 
       (princ "nil list"))

(split '() 
       (format t "list with head == ~a and tail == ~a~%" head tail) 
       (princ "nil list"))

(print (macroexpand `(split '(2 3) 
                            (+ x head) nil)))


(print (my-length '(1 2 3 4 5)))
(print (my-length '(1 2 3 4 5 6)))

(print (my-length2 '(1 2 3 4 5)))
(print (my-length2 '(1 2 3 4 5 6)))

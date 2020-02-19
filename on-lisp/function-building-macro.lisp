;; See compose.lisp for use of compose

(defmacro fn (expr) `#',(rbuild expr))

(defun rbuild (expr)
  (if (or (atom expr) (eq (car expr) 'lambda))
      expr
      (if (eq (car expr) 'compose)
          (build-compose (cdr expr))
          (build-call (car expr) (cdr expr)))))

(defun build-call (op fns)
  (let ((g (gensym)))
    `(lambda (,g)
       (,op ,@(mapcar #'(lambda (f)
                          `(,(rbuild f) ,g))
                      fns)))))

(defun build-compose (fns)
  (let ((g (gensym)))
    `(lambda (,g)
       ,(labels ((rec (fns)
                   (if fns
                       `(,(rbuild (car fns))
                          ,(rec (cdr fns)))
                       g)))
          (rec fns)))))


;; Test 
(format t "Testing function-building-macros~%")

(defvar odd-integer-p)
(setq odd-integer-p (fn (and integerp oddp)))

(write (funcall odd-integer-p 3))
(format t "~%")
(write (funcall odd-integer-p 4))

(defvar composed)
(setq composed (fn (compose (lambda (x) (+ x 3)) truncate)))

(format t "~%")
(write (funcall composed 3.3))

(print 'function-building-macro)

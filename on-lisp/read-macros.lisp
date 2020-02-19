;; Requires
(load "mapping.lisp")
(load "function-building-macro.lisp")


;; Read macro for constant functions
(set-dispatch-macro-character #\# #\?
                              #'(lambda (stream char1 char2)
                                  `#'(lambda (&rest ,(gensym))
                                       ,(read stream t nil t))))


;; Constant function example
(print (mapcar #?2 '(a b c)))


;; Delimiters. Not sure what's wrong here
;;(set-dispatch-macro-character #\] (get-macro-character #\)))


;;
(set-dispatch-macro-character #\# #\[
                              #'(lambda (stream char1 char2)
                                  (let ((accum nil)
                                        (pair (read-delimited-list #\] stream t)))
                                    (do ((i (ceiling (car pair)) (1+ i)))
                                        ((> i (floor (cadr pair)))
                                         (list 'quote (nreverse accum)))
                                      (push i accum)))))

;; Example of delimiters
;;(print #[2 7])


;; Macro for defining delimiter read-macros
(defmacro defdelim (left right parms &body body)
  `(ddfn ,left ,right #'(lambda ,parms ,@body)))


(let ((rpar (get-macro-character #\) )))
  (defun ddfn (left right fn)
    (set-macro-character right rpar)
    (set-dispatch-macro-character #\# left
                                  #'(lambda (stream char1 char2) 
                                    (apply fn
                                           (read-delimited-list right stream t))))))


;; Example of defdelim
(defdelim #\[ #\] (x y)
          (list 'quote (mapa-b #'identity (ceiling x) (floor y))))

(print #[2 7])


;; Read macro for functional composition
(defdelim #\{ #\} (&rest args)
          `(fn (compose ,@args)))

;; Example of composition
(defun double (x) (* 2 x))
(defun square (x) (* x x))
(print (funcall #{ double square 1+ } 7))

(print 'read-macros)

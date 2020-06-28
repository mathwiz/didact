(define-condition foo () ()
  (:report (lambda (condition stream)
             (princ "Stop FOOing around, numbskull!" stream))))


(defun bad-function ()
  (error 'foo))


;; Exmples to run in REPL
(handler-case (bad-function)
  (foo () "somebody signaled foo!")
  (bar () "somebody signaled bar!"))


(unwind-protect (/ 1 0)
  (princ "I need to do this even on an error."))


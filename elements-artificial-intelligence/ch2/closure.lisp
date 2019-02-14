(let (pseudo-global-variable)
  (defun get-psv ()
    "Returns a value unavailable outside the LET."
    pseudo-global-variable)
  (defun put-psv (new-value)
    "Stores to a variable outside the LET."
    (setf pseudo-global-variable new-value)))


;; Try this
;; (put-psv 5)
;; (get-psv)
;; pseudo-global-variable


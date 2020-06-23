(defparameter foo (make-string-output-stream))

(princ "This will go into foo. " foo)

(princ "This will also go into foo. " foo)

;; then run this
; (get-output-stream-string foo)


;; another with- macro
(with-output-to-string (*standard-output*)
  (princ "the sum of ")
  (princ 5)
  (princ " and ")
  (princ 2)
  (princ " is ")
  (princ (+ 5 2)))



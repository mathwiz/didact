(defun mkstr (&rest args)
  (with-output-to-string (s)
    (dolist (a args) (princ a s))))

;; Usage: (mkstr pi " is pi")


(defmacro set-nil (var)
  "Macro to set VAR to nil."
  `(setf ,var nil)
  )

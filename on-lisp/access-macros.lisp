(defmacro propmacro (popname)
  `(defmacro ,propname (obj)
     `(get ,obj ',',propname)))


(defmacro propmacros (&rest props)
  `(progn
     ,@ (mapcar #'(lambda (p) `(propmacro ,p))
                props)))


(defmacro simple-rotatef (a b)
  `(let ((first ,a)
         (second ,b))
     (setf ,a second)
     (setf ,b first))
)
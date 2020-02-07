(defmacro abbrev (short long)
  `(defmacro ,short (&rest args)
     `(,',long ,@args)))


(defmacro abbrevs (&rest names)
  `(progn
     ,@ (mapcar #'(lambda (pair)
                    `(abbrev ,@pair))
                (group names 2))))


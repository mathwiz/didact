(defmacro abbrev (short long)
  `(defmacro ,short (&rest args)
     `(,',long ,@args)))


(defmacro abbrevs (&rest names)
  `(progn
     ,@ (mapcar #'(lambda (pair)
                    `(abbrev ,@pair))
                (group names 2))))


;; Example
(abbrev collect mapcar)

(abbrevs dbind desctructuring-bind
         mvbind multiple-value-bind
         mvsetq multiple-value-setq)

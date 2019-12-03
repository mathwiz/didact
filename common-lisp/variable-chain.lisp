(defmacro variable-chain (first &rest vars)
  (labels ((recur (f r acc)
                  (cond ((null r) `(progn ,@acc))
                        (t (recur (car r) (cdr r) (cons `(setf ,f ',(car r)) acc))))))
    (recur first vars nil))
  )


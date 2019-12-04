(defun compile-arc (arc)
  `((equal this-input ',(arc-label arc))
    (format t "~&~A" ,(arc-action arc))
    (,(node-name (arc-to arc)) (rest input-syms))))

(defun compile-node (n)
  (let ((clauses (mapcar #'compile-arc
                         (node-outputs n))))
  `(defun ,(node-name n) (input-syms &aux (this-input (first input-syms)))
     (cond ((null input-syms) ',(node-name n))
           ,@clauses
           (t (error "No arc from ~A with label ~A."
                     ',(node-name n) this-input))))
  ))

(defun compile-machine (nodes)
  (let ((funcs (mapcar #'compile-node
                       nodes)))
    (mapcar #'(lambda (var)
                (eval var))
            funcs)
    `(compiled ,@nodes)
    ))


;; Try this

;;(start '(dime dime dime gum-button))


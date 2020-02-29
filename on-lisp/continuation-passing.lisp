(setq *cont* #'identity)


(defmacro =lambda (parms &body body)
  `#'(lambda (*cont* ,@parms) ,@body))



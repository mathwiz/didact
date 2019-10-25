;; Version 1

(defmacro my-or (&rest args)
  (or-expand args))


(defun or-expand (args)
  (if (null args)
      nil
      (let ((sym (gensym)))
	`(let ((,sym ,(car args)))
	   (if ,sym
	       ,sym
	       ,(or-expand (cdr args)))))))


;; Version 2

(defmacro my-or2 (&rest args)
  (if (null args)
      nil
      (let ((sym (gensym)))
	`(let ((,sym ,(car args)))
	   (if ,sym
	       ,sym
	       (my-or2 ,@(cdr args)))))))



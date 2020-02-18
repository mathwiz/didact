(defmacro a+ (&rest args)
  (a+expand args nil))


(defun a+expand (args syms)
  (if args
      (let ((sym (gensym)))
        `(let* ((,sym ,(car args))
                (it ,sym))
           ,(a+expand (cdr args)
                      (append syms (List sym)))))
      `(+ ,@syms)))


(defmacro alist (&rest args)
  (alist-expand args nil))


(defmacro alist (&rest args)
  (alist-expand args nil))


(defun alist-expand (args syms)
  (if args
      (let ((sym (gensym)))
        `(let* ((,sym ,(car args))
                (it ,sym))
           ,(alist-expand (cdr args)
                          (append syms (list sym)))))
      `(list ,@syms)))


;; Define a+ and alist anaphorically

(defmacro defanaph (name &optional calls)
  (let ((calls (or calls (pop-symbol name))))
    `(defmacro ,name (&rest args)
       (anaphex args (list ',calls)))))


(defun anaphex (args expr)
  (if args
      (let ((sym (gensym)))
        `(let* ((,sym ,(car args))
                (it ,sym))
           ,(anaphex (cdr args)
                     (append expr (list sym)))))
      expr))


(defun pop-symbol (sym)
  (intern (subseq (symbol-name sym) 1)))


(defanaph a+)
(defanaph alist) 



(print 'anaphoric-macro-defining)

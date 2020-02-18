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


;; Enhance defanaph to operate as specified by rule

(defmacro defanaph (name &optional &key calls (rule :all))
  (let* ((opname (or calls (pop-symbol name)))
          (body (case rule
                  (:all `(anaphex1 args '(,opname)))
                  (:first `(anaphex2 ',opname args))
                  (:place `(anaphex3 ',opname args)))))
    `(defmacro ,name (&rest args)
       ,body)))


(defun anaphex1 (args call)
  (if args
      (let ((sym (gensym)))
        `(let* ((,sum ,(car args))
                (it ,sym))
           ,(anaphex1 (cdr args)
                      (append call (list sym)))))
      call))


(defun anaphex2 (op args)
  `(let ((it ,(car args))) (,op it ,@ (cdr args))))


(defun anaphex3 (op args)
  `(_f (lambda (it) (,op it ,@ (cdr args))) ,(car args)))


(defanaph alist) 
(defanaph aif :rule :first)
(defanaph asetf :rule :place)



(print 'anaphoric-macro-defining)

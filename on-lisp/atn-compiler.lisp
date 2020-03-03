;; Require
(load "continuation-passing.lisp")
(load "nondeterminism.lisp")


(defmacro defnode (name &rest arcs)
  `(=defun ,name (pos regs) (choose ,@arcs)))


(defmacro down (sub next &rest cmds)
  `(=bind (* pos regs) (,sub pos (cons nil regs))
          (,next pos ,(compile-cmds cmds))))


(defmacro cat (cat next &rest cmds)
  `(if (= (length *sent*) pos)
       (fail)
       (let ((* (nth pos *sent*)))
         (if (member ',cat (types *))
             (,next (1+ pos) ,(compile-cmds cmds))
             (fail)))))


(defmacro jump (next &rest cmds)
  `(,next pos ,(compile-cmds cmds)))


(defun compile-cmds (cmds)
  (if (null cmds)
      'regs
      `(,@ (car cmds) ,(compile-cmds (cdr cmds)))))


(defmacro up (expr)
  `(let ((* (nth pos *sent*)))
     (=values ,expr pos (cdr regs))))


(defmacro getr (key &optional (regs 'regs))
  `(let ((result (cdr (assoc ',key (car ,regs)))))
     (if (cdr result) result (car result))))


(defmacro set-register (key val regs)
  `(cons (cons (cons ,key ,val) (car ,regs)) (cdr ,regs)))


(defmacro setr (key val regs)
  `(set-register ',key (list ,val) ,regs))


(defmacro pushr (key val regs)
  `(set-register ',key
                 (cons ,val (cdr (assoc ',key (car ,regs))))
                 ,regs))





;; Require
(load "atn-compiler.lisp")


(defun types (word)
  (case word
    ((do does did) '(aux v))
    ((time times) '(n v))
    ((fly flies) '(n v))
    ((like) '(v prep))
    ((liked likes) '(v))
    ((a an the) '(det))
    ((arrow arrows) '(n))
    ((i you he she him her it) '(pron))
))


(defnode mods
    (cat n mods/n
         (setr mods *)))


(defnode mods/n
    (cat n mods/n
         (pushr mods *))
  (up `(n-group ,(getr mods))))


(defnode np
    (cat det np/det
         (setr det *))
  (jump np/det
        (setr det nil))
  (cat pron pron
       (setr n *)))


(defnode pron
    (up `(np (pronoun ,(getr n)))))


(defnode np/det
    (down mods np/mods
          (setr mods *))
  (jump np/mods
        (setr mods nil)))


(defnode np/mods
    (cat n np/n
         (setr n *)))


(defnode np/n
    (up `(np (det ,(getr det))
             (modifiers ,(getr mods))
             (noun ,(getr n))))
  (down pp np/pp
        (setr pp *)))


(defnode np/pp
    (up `(np (det ,(getr det))
             (modifiers ,(getr mods))
             (noun ,(getr n))
             ,(getr pp))))


(print 'atn-sample)

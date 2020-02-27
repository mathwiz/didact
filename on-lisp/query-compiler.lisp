;; Require
(load "pattern-matching.lisp")


;; Database
(defun make-db (&optional (size 100))
  (make-hash-table :size size))


(defvar *default-db* (make-db))


(defun clear-db (&optional (db *default-db*))
  (clrhash db))


(defmacro db-query (key &optional (db '*default-db*))
  `(gethash ,key ,db))


(defun db-push (key val &optional (db *default-db*))
  (push val (db-query key db)))


(defmacro fact (pred &rest args)
  `(progn (db-push ',pred ',args)
          ',args))



;; Testing
(clear-db)

(fact paiter hogarth william english)
(fact painter reynolds joshua english)
(fact painter canale antonio venetian)
(fact dates hogarth 1697 1772)
(fact dates canale 1697 1768)
(fact dates reynolds 1723 1792)

(print (db-query 'painter))

(print 'query-compiler)

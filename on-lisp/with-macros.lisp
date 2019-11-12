;; version 1
(defmacro with-db (db &body body)
  (let ((temp (gensym)))
    `(let ((,temp *db*))
       (unwind-protect
	    (progn
	      (setq *db* ,db)
	      (lock *db*)
	      ,@body)
	    (progn
	      (release *db*)
	      (setq *db* ,temp))))))


;; version 2
(defmacro with-db (db &body body)
  (let ((gbod (gensym)))
    `(let ((,gbod #'(lambda () ,@body)))
       (declare (dynamic-extent ,gbod))
       (with-db-fn *db* ,db ,gbod))))


(defun with-db-fn (old-db new-db body)
  (unwind-protect
    (progn
	 (setq *db* new-db)
	 (lock *db*)
	 (funcall body))
    (progn
      (release *db*)
      (setq *db* old-db))))

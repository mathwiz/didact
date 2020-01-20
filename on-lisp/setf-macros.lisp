(defmacro _f (op place &rest args)
  (multiple-value-bind (vars forms var set access)
      (get-setf-method place)
    `(let* (,@(mapcar #'list vars forms)
	    (,(car var) (,op ,access ,@args)))
       ,set)))


(defmacro pull (obj place &rest args)
  (multiple-value-bind (vars forms var set access)
      (get-setf-method place)
    (let ((g (gensym)))
      `(let* ((,g ,obj)
              ,@ (mapcar #'list vars forms)
              (,(car var) (delete ,g ,access ,@args)))
         ,set))))


(defmacro pull-if (test place &rest args)
  (multiple-value-bind (vars forms var set access)
      (get-setf-method place)
    (let ((g (gensym)))
      `(let* ((,g ,test)
              ,@ (mapcar #'list vars forms)
              (,(car var) (delete-if ,g ,access ,@args)))
         ,set))))


(defmacro popn (n place)
  (multiple-value-bind (vars forms var set access)
      (get-setf-method place)
    (with-gensyms (gn glst)
      `(let* ((,gn ,n)
              ,@ (mapcar #'list vars forms)
              (,glst ,access)
              (,(car var) (nthcdr ,gn ,glst)))
         (prog1 (subseq ,glst 0 ,gn)
           ,set)))))


(defun save-data (file data)
  (with-open-file (stream file :direction :output)
    (print data stream)))


(defun read-data (file)
  (with-open-file (stream file :direction :input)
    (read stream)))


(defparameter *my-hashtable* (make-hash-table :test #'equal))


(defun add-data (key val)
  (setf (gethash key *my-hashtable*) val))


(defun get-data (key)
  (let ((result (gethash key *my-hashtable*)))
    (if result
        result
        (format t "key ~A not found" key))))


(defun enter-data ()
  (format t "~%Enter key, or (q)uit: ")
  (let ((key (read)))
    (cond ((equal key 'q) 'bye)
          (t (progn 
               (format t "~%Enter value: ")
               (let ((val (read)))
                 (add-data key val))
               (enter-data))))))



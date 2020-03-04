;; Require
(load "object-oriented.lisp")


(setq rectangle (obj))
(defprop height)
(defprop width)
(defmeth (area) rectangle (r)
         (* (height r) (width r)))


;; try it
(format t "~%Calculating area...~%")
(let ((myrec (obj rectangle)))
  (setf (height myrec) 2 (width myrec) 3)
  (format t "Area is ~D~%" (area myrec)))


(setq filesystem (obj))
(defmeth (backup :before) filesystem (fs)
         (format t "Remember to mount the tape.~%"))
(defmeth (backup) filesystem (fs)
         (format t "Oops, deleted all your files.~%")
         'done)
(defmeth (backup :after) filesystem (fs)
         (format t "Well, that was easy.~%"))


;; try it
(format t "~%Backing up...~%")
(backup (obj filesystem))

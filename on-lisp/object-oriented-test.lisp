;; Require
(load "object-oriented.lisp")


(setq rectangle (obj))
(defprop height)
(defprop width)
(defmeth (area) rectangle (r)
         (* (height r) (width r)))


;; try it
(let ((myrec (obj rectangle)))
  (setf (height myrec) 2 (width myrec) 3)
  (print (area myrec)))


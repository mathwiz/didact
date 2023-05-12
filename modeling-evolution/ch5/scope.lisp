(defun println (s) (format t "~S ~%" s))

(defun fun1 (x a b)
  (+ (* a x) b))

;; Does not compile
;; (defun fun2 (x)
;;   (+ (* a x) b))

(println "all arguments supplied")
(defun all-args ()
  (defun recur (x a)
    (cond ((= x 3) t)
          (t (progn (println (fun1 x a (* a 2)))
                    (cond ((= a 3)
                           (recur (1+ x) 1))
                          (t (recur x (1+ a))))))))
  (recur 1 1))
(all-args)


(println "using locally supplied lambda; context has name a")
(defun local-lambda ()
  (defun recur (x a b)
    (defun fun (x)
      (+ (* a x) b))
    (cond ((= x 3) t)
          (t (progn
               (println (fun x))
               (cond ((= a 3) 
                      (recur (1+ x) 1 2) ) 
                     (t (let ((next-a (1+ a))) 
                          (recur x next-a (* 2 next-a)))))))))
  (recur 1 1 2))
(local-lambda)

(println "Using stored lambdas that capture local value b. Note that context has name a with wrong value.")
(defun stored-lambda ()

(defun make-funs (a b acc)
                  (cond ((= a 4) (reverse acc)) 
                        (t (let ((next-a (1+ a)))
                                (make-funs
                                 next-a
                                 (* 2 next-a)
                                 (cons (lambda (x) (+ (* a x) b)) acc))))))

 (let* (
        (the-funs (make-funs 1 2 '()))
        (amax (1- (length the-funs)))
        )
      (defun recur (x a funs)
        (cond ((= x 3) t)
              (t (progn
                   (println (funcall (car funs) x))
                   (cond ((> a 0) (recur x (1- a) (cdr funs)))
                         (t (recur (1+ x) amax the-funs)))
                   ))
              )
        )
      (recur 1 amax the-funs )
      )
)
(stored-lambda)

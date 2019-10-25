;; Version 1

(defmacro my-nth (n lst)
  `(nth-fn ,n ,lst))


(defun nth-fn (n lst))


;; Version 2

(defmacro my-nth2 (n lst)
  `(do ((n2 ,n (1- n2))
	(lst2 ,lst (cdr lst2)))
       ((= n2 0) (car lst2))))


;; Version 3

(defmacro my-nth3 (n lst)
  `(labels ((nth-fn (n lst)
	      (if (= n 0)
		  (car lst)
		  (nth-fn (- n 1) (cdr lst)))))
     (nth-fn ,n ,lst)))



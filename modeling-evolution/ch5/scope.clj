(defn fun1 [x a b]
  (+ (* a x) b))

;; Does not compile
;; (defn fun2 [x]
;;   (+ (* a x) b))

(println "all arguments supplied")
(defn all-args []
  (let [looper (fn [x a]
                 (cond (= x 3) true
                       :else (do
                       (println (fun1 x a (* a 2)))
                       (cond (= a 3) (recur (inc x) 1)
                             :else (recur x (inc a))))))
        ]
    (looper 1 1)))
(all-args)

(println "using locally supplied lambda; context has name a")
(defn local-lambda []
  (let [looper (fn [x a b]
                 (defn fun [x] (+ (* a x) b))
                 (cond (= x 3) true
                       :else (do
                       (println (fun x))
                       (cond (= a 3) (recur (inc x) 1 2)
                             :else (let [next-a (inc a)]
                                     (recur x next-a (* 2 next-a)))))
                       ))
        ]
    (looper 1 1 2)))
(local-lambda)

(println "Using stored lambdas that capture local value b. Note that context has name a with wrong value.")

(let [make-funs (fn [a b acc]
                  (cond (= a 4) (reverse acc)
                        :else (let [next-a (inc a)]
                                (recur
                                 next-a
                                 (* 2 next-a)
                                 (cons (fn [x] (+ (* a x) b)) acc)))))
      the-funs (make-funs 1 2 '())

      stored-lambda (fn [x a funs amax]
                      (cond (= x 3) true
                            :else (do
                                    (println ((first funs) x))
                                    (cond (> a 0) (recur x (dec a) (rest funs) amax)
                                          :else (recur (inc x) amax the-funs amax)
                                          )
                                    )
                            )
                      )
      ]

  (let [limit-of-a (dec (count the-funs))
        ]
    (stored-lambda 1 limit-of-a the-funs limit-of-a)
    )
)

(defun throw-die () 
  (1+ (random 6)))

(defun throw-dice () 
  (list (throw-die) 
        (throw-die)))

(defun snake-eyes-p 
    (throw) 
  (and (= (first throw) 1) 
       (= (second throw) 1)))

(defun boxcars-p 
    (throw) 
  (and (= (first throw) 6) 
       (= (second throw) 6)))

(defun instant-loss-p 
    (throw) 
  (let* ((d1 (first throw)) 
         (d2 (second throw)) 
         (sum (+ d1 d2))) 
    (or (= sum 2) 
        (= sum 3) 
        (= sum 12))))

(defun instant-win-p 
    (throw) 
  (= (+ (first throw) 
        (second throw)) 7))


(defun say-throw 
    (throw) 
  (cond ((snake-eyes-p throw) 'snake-eyes) 
        ((boxcars-p throw) 'boxcars) 
        (t (+ (first throw) 
              (second throw)))))


(defun throw-result 
    (throw) 
  (let ((result (say-throw throw))) 
    (cond ((instant-win-p throw) 
           (list result 'you 'win)) 
          ((instant-loss-p throw) 
           (list result 'you 'lose)) 
          (t (list 'your 'point 'is result)))))


(defun craps () 
  (let* ((roll (throw-dice)) 
         (d1 (first roll)) 
         (d2 (second roll))) 
    (print-line d1 d2 (throw-result roll))))


(defun try-for-point (point) 
  (let* ((roll (throw-dice)) 
         (d1 (first roll)) 
         (d2 (second roll)) 
         (sum (+ d1 d2))) 
    (print-line
     d1 d2 (cond ((= sum 7) 
                  (list sum '-- 'you 'lose)) 
                 ((= sum point) 
                  (list sum '-- 'you 'win)) 
                 (t (list sum '-- 'throw 'again))))))


(defun print-line (d1 d2 result) 
  (cons 'Throw (cons d1 (cons 'and (cons d2 (cons '-- result))))))

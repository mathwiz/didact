(defvar family)
(setf family
      '((colin nil nil)
        (deirdre nil nil)
        (arthur nil nil)
        (kate nil nil)
        (frank nil nil)
        (linda nil nil)
        (suzanne colin deirdre)
        (bruce arthur kate)
        (charles arthur kate)
        (david arthur kate)
        (ellen arthur kate)
        (george frank linda)
        (hillary frank linda)
        (andre nil nil)
        (tamara bruce suzanne)
        (vincent bruce suzanne)
        (wanda nil nil)
        (ivan george ellen)
        (julie george ellen)
        (marie george ellen)
        (nigel andre hillary)
        (frederick nil tamara)
        (zelda vincent wanda)
        (joshua ivan wanda)
        (quentin nil nil)
        (robert quentin julie)
        (olivia nigel marie)
        (peter nigel marie)
        (erica nil nil)
        (yvette robert zelda)
        (diane peter erica)))

(defun father (x)
  (second (assoc x family)))

(defun mother (x)
  (third (assoc x family)))

(defun parents (x)
  (remove-if-not #'(lambda (x) x) (rest (assoc x family))))

(defun children (x)
  (mapcar #'first
          (remove-if-not #'(lambda (n)
                             (or (equal (second n) x)
                                 (equal (third n) x)))
                         family)))

(defun siblings (x)
  (remove-if #'(lambda (n) (equal n x))
             (union (children (father x))
                    (children (mother x)))))

(defun mapunion (fn xs)
  (reduce #'union (mapcar fn xs)))


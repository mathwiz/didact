(load 'tag-macro)


(defun polygon (points color)
  (tag polygon (points (format nil
                               "~{~a,~a ~}"
                               (mapcan (lambda (tp)
                                         (list (car tp) (cdr tp)))
                                       points))
                       style (svg-style color))))


(defun random-walk (value length)
  (unless (zerop length)
    (cons value
          (random-walk (if (zerop (random 2))
                           (1- value)
                           (1+ value))
                       (1- length)))))


;; Example

(with-open-file (*standard-output* "random-walk.svg"
                                   :direction :output
                                   :if-exists :supersede)
  (svg (loop repeat 10
            do (polygon (append '((0 . 200))
                                (loop for x
                                     for y in (random-walk 100 400)
                                     collect (cons x y))
                                '((400 . 200)))
                        (loop repeat 3
                             collect (random 256))))))



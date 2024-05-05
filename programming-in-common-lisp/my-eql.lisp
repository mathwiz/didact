(defun my-eql (a b)
  (if (typep a 'fixnum)
      (if (typep b 'fixnum)
          (zerop (- a b))
          'nil)
      (if (typep a 'single-float)
          (if (typep b 'single-float)
              (zerop (- a b))
              'nil)
          (eq a b))
))

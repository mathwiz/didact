(defun my-equal (x y)
  (if (atom x)
      (eql x y)
      (if (atom y)
          'nil
          (if (equal (car x) (car y))
              (equal (cdr x) (cdr y))
              'nil))))


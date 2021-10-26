(load 'recurse-macro)


(defun print-tag (name alst closingp)
  (princ #\<)
  (when closingp
    (princ #\/))
  (princ (string-downcase name))
  (mapc (lambda (att)
          (format t " ~a=\"~a\""
                  (string-downcase (car att))
                  (cdr att)))
        alst)
  (princ #\>)
)


;; Examples
;(print-tag 'mytag '((color . blue) (height . 9)) nil) (fresh-line)
;(print-tag 'p nil T) (fresh-line)


(defmacro tag (name atts &body body)
  `(progn (print-tag ',name
                    (list ,@(mapcar (lambda (x)
                                       `(cons ',(car x) ,(cdr x)))
                                    (pairs atts)))
                    nil)
         ,@body
         (print-tag ',name nil t))
  )


;; Example
;(tag mytag (color 'blue height 8))

;; Test it
;(macroexpand '(tag mytag (color 'blue height 8)))

;(tag mytag (color 'blue size 'big) (tag first_inner_tag ()) (tag second_innter_tag ()))

;(tag html () (tag body () (princ "Hello World!")))

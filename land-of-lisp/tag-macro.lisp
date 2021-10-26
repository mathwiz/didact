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


(defmacro html (&body body)
  `(tag html ()
        ,@body))

(defmacro body (&body body)
  `(tag body ()
        ,@body))


(defmacro svg (&body body)
  `(tag svg (xmlns "http://www.w3.org/2000/svg"
                   "xmlns:xlink" "http://www.w3.org/1999/xlink")
        ,@body))


(defun brightness (col amt)
  (mapcar (lambda (x)
            (min 255 (max 0 (+ x amt))))
          col))

;; Example
;(brightness '(255 0 0) -100)

(defun svg-style (color)
  (format nil
          "~{fill:rgb(~a,~a,~a);stroke:rgb(~a,~a,~a)~}"
          (append color
                  (brightness color -100))))

(defun circle (center radius color)
  (tag circle (cx (car center)
                  cy (cdr center)
                  r radius
                  style (svg-style color))))

;; Example
;(svg (circle '(50 . 50) 50 '(255 0 0)) (circle '(100 . 100) 50 '(0 0 255)))

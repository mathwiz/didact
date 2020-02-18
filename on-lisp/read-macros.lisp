;; Read macro for constant functions
(set-dispatch-macro-character #\# #\?
                              #'(lambda (stream char1 char2)
                                  `#'(lambda (&rest ,(gensym))
                                       ,(read stream t nil t))))


(print (mapcar #?2 '(a b c)))

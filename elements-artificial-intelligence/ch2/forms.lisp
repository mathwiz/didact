(setf x 2)

(functionp x)
(describe x)

(functionp 'if)
(macro-function 'setf)
(macro-function 'if)
(special-form-p 'if)
(describe 'if)


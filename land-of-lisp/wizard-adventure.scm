(define *nodes* '((living-room (you are in the living room.
                                a wizard is snoring loudly on the couch.))
                 (garden (you are in a beautiful garden.
                           there is a well in front of you.))
                 (attic (you are in the attic.
                          there is a giant welding torch in the corner.))))

(define (describe-location location nodes)
  (cadr (assoc location nodes)))
  
   
(define *edges* '((living-room (garden west door)
                    			(attic upstairs ladder))
                  (garden (living-room east door))
                  (attic (living-room downstairs ladder))))

(define (describe-path edge)
  `(there is a ,(caddr edge) going ,(cadr edge) from here.))


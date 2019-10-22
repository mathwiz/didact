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

  
(define (describe-paths location edges)
  (apply append (map describe-path (cdr (assoc location edges)))))


(define *objects* '(whiskey bucket frog chain))


(define *object-locations* '((whiskey living-room)
                              (bucket living-room)
                              (chain garden)
                              (frog garden)))


(define (objects-at loc objs obj-locs)
  (let ((at-loc? (lambda (obj)
                   (eq? (cadr (assoc obj obj-locs)) loc))))
	(filter at-loc? objs)))


(define (describe-objects loc objs obj-loc)
  (let ((describe-obj (lambda (obj)
                        `(you see a ,obj on the floor.))))
	(apply append (map describe-obj (objects-at loc objs obj-loc)))))	                             	                   
                                                

(define *location* 'living-room)


(define (look)
  (append 
    (describe-location *location* *nodes*)
    (describe-paths *location* *edges*)
    (describe-objects *location* *objects* *object-locations*)))
    


(define (walk direction)
  (let* ((valid-edges (cdr (assoc *location* *edges*)))
         (next (filter (lambda (it) (eq? direction (cadr it))) valid-edges)))
    (if (null? next)
      '(you cannot go that way)
      (begin
        (set! *location* (caar next))
        (look)))))


                                
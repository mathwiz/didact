; Pseudocode to move robot counterclockwise around block while drawing a square.
; Robot starts directly below 1 meter block while pointing North.

(print
(list
'(lower pen)
'(turn right 90 degrees)
'(move forward .75 meter)
'(turn left 90 degrees)
'(move forward 1.25 meter)
'(turn left 90 degrees)
'(move forward 1.25 meter)
'(turn left 90 degrees)
'(move forward 1.25 meter)
'(turn left 90 degrees)
'(move forward .75 meter)
'(raise pen)
)
)

FileStream fileIn: 'DrunkenCockroach.gst'.

| roach results avg |

DrunkenCockroach initialize.
roach := DrunkenCockroach new.

'* Run 10 experiments with a 5 x 5 room' displayNl.

results := OrderedCollection new: 10.

10 timesRepeat: 
   [ roach walkWithin: (1@1 corner: 5@5) startingAt: (1@1).
     results add: roach numberOfSteps ].

"Use 10.0e for math operations to avoid error: The program attempted to divide a number by zero"

avg := (results inject: 0 into: [ :sum :each | sum + each ]) / 10.0e. 

results displayNl.
('* It takes on average ', avg asString, ' steps') displayNl.






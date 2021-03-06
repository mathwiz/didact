(defvar ROOMS)

(setf ROOMS
      '((living-room (north front-stairs) (south dining-room)
         (east  kitchen)) 
        (upstairs-bedroom (west library) (south front-stairs)) 
        (dining-room (west downstairs-bedroom) (north living-room)
         (east pantry)) 
        (kitchen (west living-room) (south pantry)) 
        (pantry (north kitchen) (west dining-room)) 
        (downstairs-bedroom (east dining-room) (north back-stairs)) 
        (back-stairs (north library) (south downstairs-bedroom))
        (front-stairs (north upstairs-bedroom) (south living-room))
        (library (south back-stairs) (east upstairs-bedroom))))

Object subclass: Tile [
| location floorArea |

location [
    "Location of the tile on the floor"
    ^location
]

location: aPoint [
    location := aPoint.
]

floorArea: aRectangle [
    "Set the floor area to the rectangular area of the arugument"
    floorArea := aRectangle.
]

neighborAt: deltaPoint [
    "Create a new tile that is at the location of the receiver changed by the x and y amounts"
    "represented by the argument. "
    | newTile |
    newTile := Tile new floorArea: floorArea.
    newTile location: ((location + deltaPoint max: floorArea origin)
                           min: floorArea corner).
    ^newTile
]

= aTile [
    ^(aTile isKindOf: Tile) and: [ location = aTile location ]
]

hash [
    ^location hash
]

] "Tile"


Object subclass: DrunkenCockroach [
| currentTile tilesVisited |

walkWithin: aRectangle startingAt: aPoint [
    | numberTiles |
    tilesVisited := Bag new.
    currentTile location: aPoint.
    currentTile floorArea: aRectangle.
    numberTiles := (aRectangle width + 1) * (aRectangle height + 1).
    tilesVisited add: currentTile.
    [ tilesVisited asSet size < numberTiles ] whileTrue:
        [ currentTile := currentTile neighborAt: self randomStep.
          tilesVisited add: currentTile ].
]

randomStep [
    ^self class randomStep
]

numberOfSteps [
    ^tilesVisited size
]

timesSteppedOn: aTile [
    ^tilesVisited occurrencesOf: aTile
]

setVariables [
    currentTile := Tile new.
    tilesVisited := Bag new.
]

] "DrunkenCockroach"

DrunkenCockroach class extend [
| Directions Randomizer |

initialize [
    Directions := OrderedCollection new: 9.
    (-1 to: 1) do: [ :x | (-1 to: 1) do: [ :y | Directions add: x@y ] ].

    Randomizer := SampleSpaceWithReplacement data: Directions.
]

new [
    ^super new setVariables
]

randomStep [
    ^Randomizer next
]

] "class methods"


Object subclass: Card [
| suit rank |

Card class >> suit: aSymbol rank: anInteger [
    ^self basicNew setSuit: aSymbol rank: anInteger
]

suit [
    ^suit
]

rank [
    ^rank
]

asString [
    ^(self rank asString, ' ', self suit asString)
]

setSuit: aSymbol rank: anInteger [
    suit := aSymbol.
    rank := anInteger.
]

] "Card"


Object subclass: CardDeck [
| cards |

next [
    ^cards removeFirst
]

return: aCard [
    "return to the bottom of the deck"

    cards addLast: aCard
]

shuffle [
    | sample tempDeck |

    sample := SampleSpaceWithoutReplacement data: cards.
    tempDeck := OrderedCollection new: cards size.
    cards size timesRepeat: [ tempDeck addLast: sample next ].
    self cards: tempDeck.
]

isEmpty [
    ^cards isEmpty
]

cards: aCollection [
    cards := aCollection.
]

] "CardDeck"

CardDeck class extend [
| Initial |

initialize [
    "Create an unshuffled deck"

    Initial := OrderedCollection new: 52.
    #(#heart #club #spade #diamond) do: 
        [ :suit | 1 to: 13 do: 
                      [ :n | Initial add: (Card suit: suit rank: n) ] 
        ].
]

new [
    ^super new cards: Initial copy
]

] "CardDeck class methods"


Object subclass: CardHand [
| cards |

CardHand class >> new [
    ^super new setCards
]

take: aCard [
    cards add: aCard.
]

returnAllCardsTo: cardDeck [
    cards do: [ :eachCard | cardDeck return: eachCard ].
    self setCards.
]

points [
    "The sum of the ranks"

    ^cards inject: 0 into: [ :value :nextCard | value + nextCard rank ]
]

asString [
    ^cards inject: '' 
           into: [ :cardString :card | cardString isEmpty ifTrue: [ card asString ] 
                                                          ifFalse: [ cardString, ', ', card asString ] ]
]

setCards [
    cards := OrderedCollection new.
]

] "CardHand"

FileStream fileIn: 'Cards.gst'.

| cardDeck cards aCard players winners |
"
cardDeck := OrderedCollection new: 52.

#(#heart #club #spade #diamond) do: 
[ :suit | 1 to: 13 do: 
[ :n | cardDeck add: (Card suit: suit rank: n) ] 
].


cards := SampleSpaceWithoutReplacement data: cardDeck.

aCard := (cards next).
aCard asString  displayNl.
"

'* Shuffle a deck' displayNl.

CardDeck initialize.
cardDeck := CardDeck new.
cardDeck printNl.
cardDeck shuffle.

'* Pick 5 cards' displayNl.
cardDeck next asString displayNl.
cardDeck next asString displayNl.
cardDeck next asString displayNl.
cardDeck next asString displayNl.
cardDeck next asString displayNl.


'* Play a game' displayNl.
players := Set new.
4 timesRepeat: [ players add: CardHand new ].
gameCards := CardDeck new.
gameCards shuffle.

[ winners := players select: [ :each | each points between: 18 and: 21 ].
winners isEmpty and: [ gameCards isEmpty not ]] 
whileTrue: [ players do: [ :each | each points < 21 ifTrue: [ each take: gameCards next ]]].

('There are ', (winners size asString), ' winners!') displayNl.

'Here are the winning hands:' displayNl.
winners do: 
[ :hand | hand asString displayNl ]. 

'* Get ready to play again' displayNl.
players do: [ :each | each returnAllCardsTo: gameCards ].
gameCards shuffle.




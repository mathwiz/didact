FileStream fileIn: 'Cards.gst'.

| cardDeck cards aCard |

cardDeck := OrderedCollection new: 52.

#(#heart #club #spade #diamond) do: 
[ :suit | 1 to: 13 do: 
[ :n | cardDeck add: (Card suit: suit rank: n) ] 
].


cards := SampleSpaceWithoutReplacement data: cardDeck.

aCard := (cards next).
aCard asString  displayNl.

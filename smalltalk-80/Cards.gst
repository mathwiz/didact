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

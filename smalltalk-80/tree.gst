FileStream fileIn: 'BinaryTree.gst'.


Node subclass: WordNode [
| word |

WordNode class >> for: aString [
    ^self new word: aString
]

WordNode class >> for: aString left: lNode right: rNode [
    | newNode |
    newNode := super left: lNode right: rNode.
    ^newNode word: aString; yourself
]

word [
    ^word
]

word: aString [
    word := aString
]

= aWordNode [
    ^(aWordNode isKindOf: WordNode) and: [ word = aWordNode word ]
]

hash [
    ^word hash
]

] "WordNode"


| words |

words := Tree new.

words add: (WordNode for: 'cat').
words size displayNl.

words addFirst: (WordNode for: 'frog').
words size displayNl.

words addLast: (WordNode for: 'horse' left: (WordNode for: 'monkey') right: nil).
words size displayNl.

words addFirst: (WordNode for: 'ape').
words size displayNl.

words remove: (WordNode for: 'horse').
words size displayNl.

words remove: (WordNode for: 'frog').
words size displayNl.



Object subclass: Node [
| leftNode rightNode |

Node class >> left: lNode right: rNode [
    | newNode |
    newNode := self new.
    newNode left: lNode.
    newNode right: rNode.
    ^newNode
]

Node class >> nodeSize: aNode [
    ^aNode isNil
        ifTrue: [ 0 ]
        ifFalse: [ aNode size ]
]

isLeaf [
    ^leftNode isNil & rightNode isNil
]

left [
    ^leftNode
]

left: aNode [
    leftNode := aNode
]

right [
    ^rightNode
]

right: aNode [
    rightNode := aNode
]

size [
    ^ 1 +
        (self class nodeSize: leftNode) +
        (self class nodeSize: rightNode)
]

end [
    | aNode |
    aNode := self.
    [ aNode right isNil ] whileFalse: [ aNode := aNode right ].
    ^aNode
]

remove: subnode ifAbsent: exceptionBlock [
    "Assumes the root, self, is not the one to remove"
    self isLeaf ifTrue: [ ^exceptionBlock value ].
    leftNode = subnode
        ifTrue: [ leftNode := leftNode rest. ^subnode ].
    rightNode = subnode
        ifTrue: [ rightNode := rightNode rest. ^subnode ].
    leftNode isNil
        ifTrue: [ ^rightNode 
                       remove: subnode 
                       ifAbsent: exceptionBlock ].
    ^leftNode
        remove: subnode
        ifAbsent: [ rightNode isNil
                        ifTrue: [ exceptionBlock value ]
                        ifFalse: [ rightNode 
                                       remove: subnode
                                       ifAbsent: exceptionBlock ] ]
]

rest [
    leftNode isNil
        ifTrue: [ ^rightNode ]
        ifFalse: [ leftNode end right: rightNode.
                   ^leftNode ]
]

do: aBlock [
    leftNode isNil ifFalse: [ leftNode do: aBlock ].
    aBlock value: self.
    rightNode isNil ifFalse: [ rightNode do: aBlock ].
]

] "Node"


SequenceableCollection subclass: Tree [
| root |

isEmpty [
    ^root isNil
]

emptyCheck [
    self isEmpty
        ifTrue: [ self error: 'Illegal action for empty Tree' ] 
]

first [
    | save |
    self emptyCheck.
    save := root.
    [ save left isNil ] whileFalse: [ save := save left ].
    ^save
]

last [
    self emptyCheck.
    ^root end
]

size [
    ^self isEmpty
        ifTrue: [ 0 ]
        ifFalse: [ root size ]
]

add: aNode [
    ^self addLast: aNode
]

addFirst: aNode [
    "If the collection is empty, then the argument becomes root."
    "Otherwise, it is the left element of the current first node."
    self isEmpty
        ifTrue: [ root := aNode ]
        ifFalse: [ self first left: aNode ].
    ^aNode
]

addLast: aNode [
    "If the collection is empty, then the argument becomes root."
    "Otherwise, it is the last element of the current root."
    self isEmpty
        ifTrue: [ root := aNode ]
        ifFalse: [ self last right: aNode ].
    ^aNode
]

remove: aNode ifAbsent: exceptionBlock [
    "First try the root, if not found, check each of the nodes."
    self isEmpty
        ifTrue: [ ^exceptionBlock value ].
    root = aNode
        ifTrue: [ root := root rest. ^aNode ]
        ifFalse: [ ^root remove: aNode ifAbsent: exceptionBlock ]
]

removeFirst [
    self emptyCheck.
    ^self remove: self first ifAbsent: []
]

removeLast [
    self emptyCheck.
    ^self remove: self last ifAbsent: []
]

do: aBlock [
    self isEmpty
        ifFalse: [ root do: aBlock ]
]

] "Tree"



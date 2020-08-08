Object subclass: Node [
| left right |

Node class >> left: lNode right: rNode [
    | newNode |
    newNode := self new.
    newNode left: lNode.
    newNode right: rNode.
    ^newNode
]

isLeaf [
    ^left isNil & right isNil
]

left [
    ^left
]

left: aNode [
    left := aNode
]

right [
    ^right
]

right: aNode [
    right := aNode
]

] "Node"


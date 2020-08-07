Object subclass: SampleSpaceWithReplacement [
| data rand |

SampleSpaceWithReplacement class >> data: aSequenceableCollection [
    ^self new setData: aSequenceableCollection
]

next [
    "randomly select an element"
    "proceed by normalizing a random num between 0 and 1 to the size of the data"

    self isEmpty
        ifTrue: [ self error: 'no values exist in the sample space' ].
    ^data at: (rand next * data size) truncated + 1
]

next: anInteger [
    "return anInteger number of elements"
    | aCollection |
    aCollection := OrderedCollection new: anInteger.
    anInteger timesRepeat: [ aCollection addLast: self next ].
    ^aCollection
]

isEmpty [
    ^self size == 0
]

size [
    ^data size
]

setData: aSequenceableCollection [
    "populate the instance variables"

    data := aSequenceableCollection.
    rand := Random new.
]

] "SampleSpaceWithReplacement"


SampleSpaceWithReplacement subclass: SampleSpaceWithoutReplacement [

SampleSpaceWithoutReplacement class >> data: aCollection [
    ^self new setData: aCollection
]

next [
    ^data remove: super next
]

setData: aCollection [
    data := aCollection asOrderedCollection.
    rand := Random new.
]

] "SampleSpaceWithoutReplacement"


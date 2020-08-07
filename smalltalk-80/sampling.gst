FileStream fileIn: 'SampleSpace.gst'.

| people athletes |

people := SampleSpaceWithReplacement data: #('sally' 'sam' 'sue' 'sarah' 'steve').
athletes := SampleSpaceWithoutReplacement data: #('mahomes' 'lebron' 'della donne' 'rafa' 'messi' 'serena').

"examples"
'With replacement' printNl.

people next printNl.
(people next: 5) printNl.

10 timesRepeat: [ people next printNl ].

Transcript cr.
'Without replacement' printNl.

athletes next printNl.

(athletes next: 5) do: [ :it | it printNl. ].

[ athletes next ] on: Exception do: [ 'Too many calls to #next!' printNl ].

'done' printNl.



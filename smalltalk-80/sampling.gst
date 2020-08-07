FileStream fileIn: 'SampleSpace.gst'.

| people athletes |

people := SampleSpaceWithReplacement data: #('sally' 'sam' 'sue' 'sarah' 'steve').
athletes := SampleSpaceWithoutReplacement data: #('mahomes' 'lebron' 'della donne' 'rafa' 'messi' 'serena').

"examples"
'With replacement' displayNl.

people next displayNl.
(people next: 5) displayNl.

10 timesRepeat: [ people next displayNl ].

Transcript cr.
'Without replacement' displayNl.

athletes next displayNl.

(athletes next: 5) do: [ :it | it displayNl. ].

[ athletes next ] on: Exception do: [ 'Too many calls to #next!' displayNl ].

'done' displayNl.



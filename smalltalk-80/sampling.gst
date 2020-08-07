| people |

people := SampleSpaceWithReplacement data: #('sally' 'sam' 'sue' 'sarah' 'steve').

"examples"
people next printNl.
(people next: 5) printNl.

10 timesRepeat: [ people next printNl ].



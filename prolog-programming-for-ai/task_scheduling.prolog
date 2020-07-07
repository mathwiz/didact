/* Problem-specific relations for task scheduling

Nodes in the state space are partial schedules specified by:

[ Task1/D1, Task2/D2, ... ] * [ Task1/F1, Task2/F2, ... ] * FinTime

The first list specifies the waiting tasks and their durations; the second list specifies
the currently executed tasks and their finishing times, ordered so that F1 <= F2, F2 <= F3 ...
FinTime is the latest completion time of current engagements of the processors.

*/

s( Tasks1 * [ _/F | Active1 ] * Fin1, Tasks2 * Active2 * Fin2, Cost ) :-
    del( Task/D, Tasks1, Tasks2 ),                       % Pick a waiting task
    \+ ( member( T/_, Tasks2 ), before( T, Task ) ),     % Check precedence
    \+ ( member( T1/F1, Active1 ), F < F1, before( T1, Task ) ), % Active tasks too
    Time is F + D,
    insert( Task/Time, Active1, Active2, Fin1, Fin2 ),
    Cost is Fin2 - Fin1.


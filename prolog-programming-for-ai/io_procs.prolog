% square
square :-
    write( 'Next item, please ("stop" to end): ' ),
    read( X ),
    process( X ).

process( stop ) :- !.

process( N ) :-
    C is N * N,
    write( 'Square of ' ),
    write( N ),
    write( ' is '),
    write( C ),
    nl,
    square.


% writelist
writelist( [] ).

writelist( [X | L] ) :-
    write( X ),
    nl,
    writelist( L ).


% write a set of *s for each number in a list
% example: bars( [3,7,11] ).
bars( [N | L] ) :-
    stars( N ),
    nl,
    bars( L ).

stars( N ) :-
    N > 0,
    write(*),
    N1 is N - 1,
    stars( N1 ).

stars( N ) :-
    N =< 0.

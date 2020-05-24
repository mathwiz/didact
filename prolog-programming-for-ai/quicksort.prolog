t( X, Y ) :-                           % X greater than Y
    X > Y.

quicksort( [], [] ).

quicksort( [X | Tail], Sorted ) :-
    split( X, Tail, Small, Big ),
    quicksort( Small, SortedSmall ),
    quicksort( Big, SortedBig ),
    conc( SortedSmall, [X | SortedBig], Sorted ).

split( X, [], [], [] ).

split( X, [Y | Tail], [Y | Small], Big ) :-
    gt( X, Y ), !,
    split( X, Tail, Small, Big ).

split( X, [Y | Tail], Small, [Y | Big] ) :-
    split( X, Tail, Small, Big ).

conc( [], L, L).

conc( [X | L1], L2, [X | L3] ) :-
    conc( L1, L2, L3 ).



% Example

% quicksort( [2,3,1,6,8,3,2,7], X ).


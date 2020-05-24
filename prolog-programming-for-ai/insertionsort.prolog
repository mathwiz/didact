gt( X, Y ) :-                           % X greater than Y
    X > Y.

insertionsort( [], [] ).

insertionsort( [X | Tail], Sorted ) :-
    insertionsort( Tail, SortedTail ),  % Sort the tail
    insert( X, SortedTail, Sorted ).

insert( X, [Y | Sorted], [Y | Sorted1] ) :-
    gt( X, Y ), !,
    insert( X, Sorted, Sorted1 ).

insert( X, Sorted, [X | Sorted] ).



% Example

% insertionsort( [2,3,1,6,8,3,2,7], X ).


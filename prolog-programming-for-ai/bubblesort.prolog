% Note: this program does not handle repeated elements properly

gt( X, Y ) :-                           % X greater than Y
    X > Y.

bubblesort( List, Sorted ) :-
    swap( List, List1 ), !,             % A useful swap in List?
    sort( List1, Sorted ).

bubblesort( Sorted, Sorted ).           % Otherwise already sorted

swap( [X, Y | Rest], [Y, X | Rest] ) :- % Swap first two elements if needed
    gt( X, Y ).

swap( [Z | Rest], [Z | Rest1] ) :-      % Swap elements in tail
    swap( Rest, Rest1 ).


% Example

% bubblesort( [2,3,1,6,8,3,2,7], X ).

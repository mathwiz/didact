% Insertion and deletion in the 2-3 dictionary

add23( Tree, X, Tree1 ) :-               % Add X to Tree giving Tree1
    ins( Tree, X, Tree1 ).               % Tree grows in breadth

add23( Tree, X, n2( T1, M2, T2 ) ) :-    % Tree grows upward
    ins( Tree, X, T1, M2, T2 ).

del23( Tree, X, Tree1 ) :-               % Delete X from Tree giving Tree1
    add23( Tree1, X, Tree ).

ins( nil, X, l(X) ).

ins( l(A), X, l(A), X, l(X) ) :-
    gt( X, A ).

ins( l(A), X, l(X), A, l(A) ) :-
    gt( A, X ).

ins( n2( T1, M, T2 ), X, n2( NT1, M, T2 ) ) :-
    gt( M, X ),
    ins( T1, X, NT1 ).

ins( n2( T1, M, T2 ), X, n3( NT1a, Mb, NT1b, M, T2 ) ) :-
    gt( M, X ),
    ins( T1, X, NT1a, Mb, NT1b ).

ins( n2( T1, M, T2 ), X, n2( N1, M, NT2 ) ) :-
    gt( X, M ),
    ins( T2, X, NT2 ).

ins( n2( T1, M, T2 ), X, n3( T1, M, NT2a, Mb, NT2b ) ) :-
    gt( X, M ),
    ins( T2, X, NT2a, Mb, NT2b ).

ins( n3( T1, M2, T2, M3, T3 ), X, n3( NT1, M2, T2, M3, T3 ) ) :-
    gt( M2, X ),
    ins( T1, X, NT1 ).

ins( n3( T1, M2, T2, M3, T3 ), X, n2( NT1a, Mb, NT1b ), M2, n2( T2, M3, T3 ) ) :-
    gt( M2, X ),
    ins( T1, X, NT1a, Mb, NT1b ).

ins( n3( T1, M2, T2, M3, T3 ), X, n3( T1, M2, NT2, M3, T3 ) ) :-
    gt( X, M2 ), gt( M3, X ),
    ins( T2, X, NT2 ).

ins( n3( T1, M2, T2, M3, T3 ), X, n2( T1, M2, NT2a ), Mb, n2( NT2b, M3, T3 ) ) :-
    gt( X, M2 ), gt( M3, X),
    ins( T2, X, NT2a, Mb, NT2b ).

ins( n3( T1, M2, T2, M3, T3 ), X, n3( T1, M2, T2, M3, NT3 ) ) :-
    gt( X, M3 ),
    ins( T3, X, NT3 ).

ins( n3( T1, M2, T2, M3, T3 ), X, n2( T1, M2, T2 ), M3, n2( NT3a, Mb, NT3b ) ) :-
    gt( X, M3 ),
    ins( T3, X, NT3a, Mb, NT3b ).



% Showing a 2-3 dictionary

show( T ) :-
    show( T, 0 ).


show( nil, _ ).

show( l(A), H ) :-
    tab( H ), write( A ), nl.

show( n2( T1, M, T2 ), H ) :-
    H1 is H + 5,
    show( T2, H1 ),
    tab( H ), write( -- ), nl,
    tab( H ), write( M ), nl,
    tab( H ), write( -- ), nl,
    show( T1, H1 ).

show( n3( T1, M2, T2, M3, T3 ), H ) :-
    H1 is H + 5,
    show( T3, H1 ),
    tab( H ), write( -- ), nl,
    tab( H ), write( M3 ), nl,
    show( T2, H1 ),
    tab( H ), write( M2 ), nl,
    tab( H ), write( -- ), nl,
    show( T1, H1 ).


nil.             % Do we need an atom for an empty tree?


gt( X, Y ) :-    % X greater than Y
    X > Y.


in( X, t( _, X, _ ) ).

in( X, t( Left, Root, Right ) ) :-
    gt( Root, X ),
    in( X, Left ).

in( X, t( Left, Root, Right ) ) :-
    gt( X, Root ),
    in( X, Right ).


addleaf( nil, X, t( nil, X, nil) ).

addleaf( t( Left, X, Right ), X, t( Left, X, Right ) ).

addleaf( t( Left, Root, Right ), X, t( Left1, Root, Right ) ) :-
    gt( Root, X ),
    addleaf( Left, X, Left1 ).

addleaf( t( Left, Root, Right ), X, t( Left, Root, Right1 ) ) :-
    gt( X, Root ),
    addleaf( Right, X, Right1 ).


del( t( nil, X, Right ), X, Right ).

del( t( Left, X, nil ), X, Left ).

del( t( Left, X, Right ), X, t( Left, Y, Right1 ) ) :-
    delmin( Right, Y, Right1 ).

del( t( Left, Root, Right ), X, t( Left1, Root, Right ) ) :-
    gt( Root, X ),
    del( Left, X, Left1 ).

del( t( Left, Root, Right ), X, t( Left, Root, Right1 ) ) :-
    gt( X, Root ),
    del( Right, X, Right1 ).

delmin( t( nil, Y, R ), Y, R ).

delmin( t( Left, Root, Right ), Y, t( Left1, Root, Right ) ) :-
    delmin( Left, Y, Left1 ).


add( D, X, D1 ) :-
    addroot( D, X, D1 ).                   % Add X as a new root

add( t( L, Y, R ), X, t( L1, Y, R ) ) :-   % Insert X into left subtree
    gt( Y, X ),
    add( L, X, L1 ).

add( t( L, Y, R ), X, t( L, Y, R1 ) ) :-   % Insert X into right subtree
    gt( X, Y ),
    add( R, X, R1 ).

addroot( nil, X, t( nil, X, nil ) ).       % Insert into empty tree

addroot( t( L, X, R ), X, t( L, X, R ) ).  % X already in tree

addroot( t( L, Y, R ), X, t( L1, X, t( L2, Y, R ) ) ) :-
    gt( Y, X ),
    addroot( L, X, t( L1, X, L2 ) ).

addroot( t( L, Y, R ), X, t( t( L, Y, R1 ), X, R2 ) ) :-
    gt( X, Y ),
    addroot( R, X, t( R1, X, R2 ) ).


show( T ) :-
    show2( T, 0 ).

show2( nil, _ ).

show2( t( L, X, R ), Indent ) :-
    Ind2 is Indent + 2,
    show2( R, Ind2 ),
    tab( Indent ), write( X ), nl,
    show2( L, Ind2 ).

    

% Examples

% in( 3, t( t( nil, 1, nil ), 2, t( t( nil, 3, nil ), 4, nil ) ) ).

% add( nil, 3, D1 ), add( D1, 5, D2 ), add( D2, 1, D3 ), add( D3, 6, D ), add( DD, 5, D ).


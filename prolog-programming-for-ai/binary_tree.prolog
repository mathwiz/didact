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



% Example

% in( 3, t( t( nil, 1, nil ), 2, t( t( nil, 3, nil ), 4, nil ) ) ).



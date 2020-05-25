t( X, Y ) :-                           % X greater than Y
    X > Y.

in( X, t( _, X, _ ) ).

in( X, t( Left, Root, Right ) ) :-
    gt( Root, X ),
    in( X, Left ).

in( X, t( Left, Root, Right ) ) :-
    gt( X, Root ),
    in( X, Right ).


% Example
% Don't know why this does not work.

% in( a, t( t( nil, b, nil ), a, t( t( nil, d, nil ), c, nil ) ) ).



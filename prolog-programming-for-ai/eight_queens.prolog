solution( [] ).

solution( [X/Y | Others] ) :-    % first queen at X/Y, rest at Others
    solution( Others ),
    member( Y, [1,2,3,4,5,6,7,8] ),
    noattack( X/Y, Others ).     % first queen does not attack others


noattack( _, [] ).

noattack( X/Y, [X1/Y1 | Others] ) :-
    Y =\= Y1,                    % different Y coordinates
    Y1 - Y =\= X1 - X,           % different diagonals 
    Y1 - Y =\= X - X1,
    noattack( X/Y, Others ).


member( X, [X | L]).

member( X, [Y | L] ) :-
    member( X, L ).


% Solution template. Each of the eight columns must appear once.
template( [1/Y1, 2/Y2, 3/Y3, 4/Y4, 5/Y5, 6/Y6, 7/Y7, 8/Y8] ).

% to solve
% template( S ), solution( S ).

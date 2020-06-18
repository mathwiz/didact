% Best-first search

bestfirst( Start, Solution ) :-
    biggest( Big ),                                        % Big > any f-value
    expand( [], l( Start, 0/0 ), Big, _, yes, Solution ).


expand( P, l( N, _ ), _, _, yes, [ N | P ] ) :-
    goal( N ).

expand( P, l( N, F/G ), Bound, Tree1, Solved, Sol ) :-
    F =< Bound,
    ( bagof( M/C, ( s( N, M, C ), \+ member( M, P ) ), Succ ), !,
      succlist( G, Succ, Ts ),
      bestf( Ts, F1 ),
      expand( P, t( N, F1/G, Ts ), Bound, Tree1, Solved, Sol );
      Solved = never ).                                    % No successors - dead end

expand( P, t( N, F/G, [ T | Ts ] ), Bound, Tree1, Solved, Sol ) :-
    F =< Bound,
    bestf( Ts, BF ), min( Bound, BF, Bound1 ),
    expand( [ N | P ], T, Bound1, T1, Solved1, Sol ),
    continue( P, t( N, F/G, [ T1 | Ts ] ), Bound, Tree1, Solved1, Solved, Sol ).

expand( _, t( _, _, [] ), _, _, never, _ ) :- !.           % A dead tree will never be solved

expand( _, Tree, Bound, Tree, no, _ ) :-
    f( Tree, F ), F > Bound.                               % Cannot grow - bound exceeded


continue( _, _, _, _, yes, yes, Sol ).

continue( P, t( N, F/G, [ T1 | Ts ] ), Bound, Tree1, Solved1, Solved, Sol ) :-
    ( Solved1 = no, insert( T1, Ts, NTs );
      Solved1 - never, NTs = Ts ),
    bestf( NTs, F1 ),
    expand( P, t( N, F1/G, NTs ), Bound, Tree1, Solved, Sol ).


succlist( _, [], [] ).

succlist( G0, [ N/C | NCs ], Ts ) :-
    G is G0 + C,
    h( N, H ),                                              % Heuristic term h(N)
    F is G + H,
    succlist( G0, NCs, Ts1 ),
    insert( l( N, F/G ), Ts1, Ts ).


% Insert T into list of trees Ts preserving order w. r. t. f-values

insert( T, Ts, [ T | Ts ] ) :-
    f( T, F ), bestf( Ts, F1 ),
    F =< F1, !.

insert( T, [ T1 | Ts ], [ T1 | Ts1 ] ) :-
    insert( T, Ts, Ts1 ).


% Extract f-value

f( l( _, F/_ ), F ).                                        % f-value of a leaf

f( t( _, F/_, _ ), F ).                                     % f-value of a tree


bestf( [ T | _ ], F ) :-                                    % Best f-value of a list of trees
    f( T, F ).

bestf( [], Big ) :-                                         % No trees: bad f-value
    biggest( Big ).


min( X, Y, X ) :-
    X =< Y, !.

min( X, Y, Y ).

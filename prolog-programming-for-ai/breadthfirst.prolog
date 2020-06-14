solve( Start, Solution ) :-
    breadthfirst( [ [Start] ], Solution ).


breadthfirst( [ [Node | Path] | _ ], [Node | Path] ) :-
    goal( Node ).

breadthfirst( [ [N | Path] | Paths ], Solution ) :-
    bagof( [M, N | Path],
           ( s( N, M ), \+ member( M, [N | Path] ) ),
           NewPaths ),                     % NewPaths = acyclic extensions of [N | Path]
    conc( Path, NewPaths, Paths1), !,
    breadthfirst( Paths1, Solution );
    breadthfirst( Paths, Solution ).       % Case that N has no successor

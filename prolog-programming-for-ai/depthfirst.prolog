solve( Node, Solution ) :-
    depthfirst( [], Node, Solution ).


depthfirst( Path, Node, [Node | Path] ) :-
    goal( Node ).

depthfirst( Path, Node, Sol ) :-
    s( Node, Node1 ),
    \+ member( Node1, Path ),                        % Prevent a cycle
    depthfirst( [Node | Path], Node1, Sol ).

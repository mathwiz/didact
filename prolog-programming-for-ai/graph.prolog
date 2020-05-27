path( A, Z, Graph, Path ) :-
    path1( A, [Z], Graph, Path ).

path1( [A | Path1], _, [A | Path1] ).

path1( A, [Y | Path1], Graph, Path ) :-
    adjacent( X, Y, Graph ),
    \+ member( X, Path1 ),                           % No-cycle condition
    path1( A, [X, Y | Path1], Graph, Path1 ).

adjacent( X, Y, graph( Nodes, Edges ) ) :-
    member( e( X, Y ), Edges );
    member( e( Y, X ), Edges ).

hamiltonian( Graph, Path ) :-
    path( _, _, Graph, Path ),
    covers( Path, Graph ).

covers( Path, Graph ) :-
    \+ node( N, Graph ), \+ member( N, Path ).


% Depth-first AND/OR search
% Procedure solve( Node, SolutionTree ) finds a solution tree
% for a node in an AND/OR graph

:- op( 600, xfx, ---> ).
:- op( 500, xfx, : ).


solve( Node, Node ) :-           % Solution tree of a goal node is Node itslef
    goal( Node ).

solve( Node, Node ---> Tree ) :-
    Node ---> or : Nodes,
    member( Node1, Nodes ),      % Node is an OR node
    solve( Node1, Tree ).        % Select a successor Node1 of Node

solve( Node, Node ---> and : Trees ) :-
    Node ---> and : Nodes,       % Node is an AND node
    solveall( Nodes, Trees ).    % Solve all Node's successors


solveall( [], [] ).

solveall( [ Node | Nodes ], [ Tree | Trees ] ) :-
    solve( Node, Tree ),
    solveall( Nodes, Trees ).


show( Tree ) :-                  % Display solution tree
    show( Tree, 0 ), !.          % Indented by 0

show( Node ---> Tree, H ) :-     % Display solution tree indented by H
    write( Node ), write( ' ---> ' ),
    H1 is H + 7,
    show( Tree, H1 ), !.

show( and : [ T ], H ) :-        % Display AND list of solution trees
    show( T, H ).

show( and : [ T | Ts ], H ) :-   % Display AND list of solution trees
    show( T, H ),
    tab( H ),
    show( and : Ts, H ), !.

show( Node, H ) :-
    write( Node ), nl.

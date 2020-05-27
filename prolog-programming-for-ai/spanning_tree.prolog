% Finding a spanning tree of a graph
%
% Trees and graphs are represented by lists of their
% edges. For example:
%        Graph = [a-b, b-c, b-d, c-d]


stree( Graph, Tree ) :-                 % Tree is a spanning tree of Graph
    member( Edge, Graph ),
    spread( [Edge], Tree, Graph ).

spread( Tree1, Tree, Graph ) :-
    addedge( Tree1, Tree2, Graph ),
    spread( Tree2, Tree, Graph ).

spread( Tree, Tree, Graph ) :-
    \+ addedge( Tree, _, Graph ).       % No edge can be added without creating a cycle

addedge( Tree, [A-B | Tree], Graph ) :-
    adjacent( A, B, Graph ),            % Nodes A and B adjacent in Graph
    node( A, Tree ),                    % A in Tree
    \+  node( B, Tree ).                % A-B doesn't create a cycle in Tree

adjacent( A, B, Graph ) :-
    member( A-B, Graph );
    member( B-A, Graph ).

node( A, Graph ) :-                     % A is a node in Graph if
    adjacent( A, _, Graph ).            % A is adjacent to anything in Graph


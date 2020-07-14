/* BEST-FIRST AND/OR SEARCH

This program only generates one solution. This solution is guaranteed to be a
cheapest one if the heuristic function used is a lower bound of the actual costs of
solution trees.

Search tree is either:

tree( Node, F, C, SubTrees )       tree of candidate solutions

leaf( Node, F, C )                 leaf of a search tree

solvedtree( Node, F, SubTrees )    solution tree

solvedleaf( Node, F )              leaf of solution tree

C is the cost of the arc pointing to Node

F = C + H, where H is the heuristic estimate of an optimal solution subtree sooted in Node

SubTrees are always ordered so that:

1) all solved subtrees are at the end of a list;

2) other (unsolved subtrees) are ordered according to ascending F-values

*/

:- op( 600, xfx, ---> ).
:- op( 500, xfx, : ).


andor( Node, SolutionTree) :-
    expand( leaf( Node, 0, 0 ), 9999, SolutionTree, yes ).  % Assume 9999 < any F-value



% Procedure expand( Tree, Bound, NewTree, Solved )
% expands Tree with Bound producing NewTree whose
% 'solved-status' is Solved

% Case 1: bound exceeded
expand( Tree, Bound, Tree, no ) :-
    f( Tree, F ), F > Bound, !.                             % Bound exceeded


% In all remaining cases F <= Bound

% Case 2: goal encountered

expand( leaf( Node, F, C ), _, solvedleaf( Node, F ), yes ) :-
    goal( Node ), !.


% Case 3: expanding a leaf

expand( leaf( Node, F, C ), Bound, NewTree, Solved ) :-
    expandnode( Node, C, Tree1 ), !,
    expand( Tree1, Bound, NewTree, Solved );
    Solved = never, !.                                      % No successors, dead end


% Case 4: expanding a tree

expand( tree( Node, F, C, SubTrees ), Bound, NewTree, Solved ) :-
    Bound1 is Bound - C,
    expandlist( SubTrees, Bound1, NewSubs, Solved1 ),
    continue( Solved1, Node, C, NewSubs, Bound, NewTree, Solved ).


% expandlist( Trees, Bound, NewTrees, Solved )
% expands tree list Trees with Bound producing
% NewTrees whose 'solved-status' is Solved

expandlist( Trees, Bound, NewTrees, Solved ) :-
    selecttree( Trees, Tree, OtherTrees, Bound, Bound1 ),
    expand( Tree, Bound1, NewTree, Solved1 ),
    combine( OtherTrees, NewTree, Solved1, NewTrees, Solved ).


% 'continue' decides how to continue after expanding a tree list

continue( yes, Node, C, SubTrees, _, solvedtree( Node, F, SubTrees ), yes ) :-
    backup( SubTrees, H ), F is C + H, !.

continue( never, _, _, _, _, _, never ) :- !.

continue( no, Node, C, SubTrees, Bound, NewTree, Solved ) :-
    backup( SubTrees, H), F is C + H, !,
    expand( tree( Node, F, C, SubTrees ), Bound, NewTree, Solved ).


% 'combine' combines results of expanding a tree and a tree list

combine( or : _, Tree, yes, Tree, yes ) :- !.               % OR list solved

combine( or : Trees, Tree, no, or : NewTrees, no ) :-
    insert( Tree, Trees, NewTrees ), !.                     % OR list still unsolved

combine( or : [], _, never, _, never ) :- !.                % No more candidates

combine( or : Trees, _, never, or : Trees, no ) :- !.       % There are more candidates

combine( and : Trees, Tree, yes, and : [ Tree | Trees ], yes ) :-
    allsolved( Trees ), !.                                  % AND list solved

combine( and : _, _, never, _, never ) :- !.                % AND list unsolvable

combine( and : Trees, Tree, YesNo, and : NewTrees, no ) :-
    insert( Tree, Trees, NewTrees ), !.                     % AND list still unsolved


% 'expandnode' makes a tree of a node and its successors

expandnode( Node, C, tree( Node, F, C, Op : SubTrees ) ) :-
    Node ---> Op : Successors,
    evaluate( Successors, SubTrees ),
    backup( Op : SubTrees, H ), F is C + H.


evaluate( [], [] ).




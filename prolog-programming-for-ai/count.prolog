% Relation
% 
% count( Term, List, N )
%
% is: if all occurrences of Term that are found in List
% we place that number in N.

count( _, [], 0 ).

count( Term, [Head | Tail], N ) :-
    Term == Head, !,
    count( Term, Tail, N1 ),
    N is N1 + 1;
    count( Term, Tail, N ).


% Examples

% count( a, [a,b,a,a], X ).

% count( a, [a,b,X,Y], Na ).

% count( b, [a,b,X,Y], Nb ).

% L = [a,b,X,Y], count( a, L, Na ), count( b, L, Nb ).

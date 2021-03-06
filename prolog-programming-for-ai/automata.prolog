final(s3).

trans(s1, a, s1).
trans(s1, a, s2).
trans(s1, b, s1).
trans(s2, b, s3).
trans(s3, b, s4).

silent(s2, s4).
silent(s3, s4).

accepts(S, []) :-
    final(S).

accepts(S, [X | Rest]) :-
    trans(S, X, S1),
    accepts(S1, Rest).

accepts(S, String) :-
    silent(S, S1),
    accepts(S1, String).


% accepts(s1, [a,a,a,b]).  -> yes
% accepts(S, [a,b]).       -> s1; s3
% accepts(s1, [X1,X2,X3]).
% String = [_,_,_], accepts(s1, String).

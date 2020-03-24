% load with consult('filename').

parent(pam, bob).
parent(tom, bob).
parent(tom, liz).
parent(bob, ann).
parent(bob, pat).
parent(pat, jim).

% parent(X, liz).
% parent(bob, X).

offspring(Y, X) :- parent(X, Y).

% keep like predicates together
male(jim).
male(tom).
male(bob).
female(liz).
female(pat).
female(ann).
female(pam).


mother(X, Y) :- 
    parent(X, Y), 
    female(X).

grandparent(X, Z) :-
    parent(X, Y),
    parent(Y, Z).

different(X, Y) :- \==(X, Y).

sister(X, Y) :-
    parent(Z, X),
    parent(Z, Y),
    female(X),
    different(X, Y).

predecessor(X, Z) :- 
    parent(X, Z).

predecessor(X, Z) :- 
    parent(X, Y), 
    predecessor(Y, Z).


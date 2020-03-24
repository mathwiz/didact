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

% gprolog does not like these. why?
female(pam).
male(tom).
male(bob).
female(liz).
female(pat).
female(ann).
male(jim).

mother(X, Y) :- 
    parent(X, Y), 
    female(X).

grandparent(X, Z) :-
    parent(X, Y),
    parent(Y, Z).

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


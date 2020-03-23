parent(pam, bob).
parent(tom, bob).
parent(tom, liz).
parent(bob, ann).
parent(pat, bob).
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

mother(X, Y) :- parent(X, Y), female(X).


% Translating a propositional formula into (asserted) clauses

:- op( 100, fy, ~ ).          % Negation
:- op( 110, xfy, & ).         % Conjunction
:- op( 120, xfy, v ).         % Disjunction
:- op( 130, xfy, => ).        % Implication


% Translate conjunctive formula
translate( F & G ) :- !,
    translate( F ),
    translate( G ).

translate( Formula ) :-
    transform( Formula, NewFormula ), !,          % Transformation step on Formula
    translate( NewFormula ).

% No more transformation possible
translate( Formula ) :-
    assert( clause( Formula ) ).


% Transformation rules for propositional formulas

transform( ~( ~X ), X ) :- !.                     % Double negation

transform( X >= Y, ~X v Y ) :- !.                 % Eliminate implication

transform( ~( X & Y ), ~X v ~Y ) :- !.            % De Morgan's law

transform( ~( X v Y ), ~X & ~Y ) :- !.            % De Morgan's law

transform( X & Y v Z, ( X v Z ) & ( Y v Z ) ) :- !.  % Distribution

transform( X v Y & Z, ( X v Y ) & ( X v Z ) ) :- !.  % Distribution

transform( X v Y, X1 v Y ) :-
    transform( X, X1 ), !.                        % Transform subexpression

transform( X v Y, X v Y1 ) :-
    transform( Y, Y1 ), !.                        % Transform subexpression

transform( ~X, ~X1 ) :-
    transform( X, X1 ).                           % Transform subexpression


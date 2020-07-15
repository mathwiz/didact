/* Procedure

explore( Goal, Trace, Answer )

finds Answer to a given Goal. Trace is a chain of ancestor goals and rules.
'explore' tends to find a positive answer to a question. Answer is 'false'
only when all the possibilities have been investigated and they all resulted
in 'false'.

*/

:- op( 900, xfx, : ).
:- op( 800, xfx, was ).
:- op( 870, fx, if ).
:- op( 880, xfx, then ).
:- op( 550, xfy, or ).
:- op( 540, xfy, and ).
:- op( 300, fx, 'derived by' ).
:- op( 600, xfx, from ).
:- op( 600, xfx, by ).


explore( Goal, Trace, Goal is true was 'found as a fact' ) :-
    fact : Goal.

% Assume only one rule about each type of goal

explore( Goal, Trace,
         Goal is TruthValue was 'derived by' Rule from Answer ) :-
    Rule : if Condition then Goal,                          % Rule relevant to Goal
    explore( Condition, [ Goal by Rule | Trace ], Answer ),
    truth( Answer, TruthValue ).

explore( Goal1 and Goal2, Trace, Answer ) :- !,
    explore( Goal1, Trace, Answer1 ),
    continue( Answer1, Goal1 and Goal2, Trace, Answer ).

explore( Goal1 or Goal2, Trace, Answer ) :-
    exploreyes( Goal1, Trace, Answer );                     % Positive answer to Goal1
    exploreyes( Goal2, Trace, Answer ).                     % Positive answer to Goal2

explore( Goal1 or Goal2, Trace, Answer1 and Answer2 ) :- !,
    \+ exploreyes( Goal1, Trace, _ ),
    \+ exploreyes( Goal2, Trace, _ ),                       % No positive answer
    explore( Goal1, Trace, Answer1 ),                       % Answer1 must be negative
    explore( Goal2, Trace, Answer2 ).                       % Answer2 must be negative

explore( Goal, Trace, Goal is Answer was told ) :-
    useranswer( Goal, Trace, Answer ).                      % User-supplied answer


exploreyes( Goal, Trace, Answer ) :-
    explore( Goal, Trace, Answer ),
    positive( Answer ).

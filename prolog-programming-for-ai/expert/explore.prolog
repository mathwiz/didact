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

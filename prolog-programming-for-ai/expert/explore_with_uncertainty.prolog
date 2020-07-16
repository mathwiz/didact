/* Procedure

explore( Goal, Trace, Answer )

finds a likelihood measure that Goal is true. Answer contains this likelihood.
Trace is the chain of ancestor goals and rules, and can be used for 'why' explanations.

*/

:- op( 900, xfx, : ).
:- op( 800, xfx, was ).
:- op( 870, fx, if ).
:- op( 880, xfx, then ).
:- op( 550, xfy, or ).
:- op( 540, xfy, and ).
:- op( 300, fx, 'derived by' ).
:- op( 300, fx, 'derived from' ).
:- op( 600, xfx, from ).
:- op( 600, xfx, by ).
:- op( 600, xfx, with ).


explore( Goal, Trace, ( Goal : Prob ) was 'derived by' RulesAnswers ) :-
    bagof( Rule : if Condition then Goal with Strength,
           Rule : if Condition then Goal with Strength,
           Rules ),                                         % All rules about Goal
    prior( Goal, Prob0 ),                                   % Prior prob of Goal
    modify( Prob0, Rules, Trace, Prob, RulesAnswers ).      % Modify prior prob


explore( Goal1 and Goal2, Trace,
         ( Goal1 and Goal2 : P ) was 'derived from' ( Answer1 and Answer2 ) ) :-
    !,
    explore( Goal1, Trace, Answer1),
    explore( Goal2, Trace, Answer2),
    probability( Answer1, P1 ),
    probability( Answer2, P2 ),
    min( P1, P2, P ).

explore( Goal1 or Goal2, Trace,
         ( Goal1 or Goal2 : P ) was 'derived from' ( Answer1 and Answer2 ) ) :-
    !,
    explore( Goal1, Trace, Answer1),
    explore( Goal2, Trace, Answer2),
    probability( Answer1, P1 ),
    probability( Answer2, P2 ),
    max( P1, P2, P ).

explore( \+ Goal, Trace, ( \+ Goal : Prob ) was 'derived from' Answer ) :-
    !,
    explore( Goal, Trace, Answer ),
    probability( Answer, P ),
    invert( P, Prob ).

explore( Goal, Trace, ( Goal : Prob )  was told ) :-
    useranswer( Goal, Trace, Prob ).                        % User-supplied answer


% Relation
%
% modify( Prob0, Rules, Trace, Prob, RulesAnswers )
%
% There is a goal Goal whose prior probability is Prob0.
% Rules bear on Goal.
% The cumulative effect of these rules (through their condition parts)
% modifies Prob0 into Goal's posterior probability Prob.
% Trace is the list of Goal's ancestor goals and rules.
% RulesAnswers are the results of analysis of the condition parts of Rules.

modify( Prob0, [], Trace, Prob0, [] ).                      % No rule - no effect

modify( Prob0,
        [ Rule : if Cond then Goal with Strength | Rules ],
        Trace, Prob, [ Rule from Answer | RulesAnswers ] ) :-
    explore( Cond, [ Goal by Rule | Trace ], Answer ),      % Condition of first rule
    prior( Cond, P0 ),
    probability( Answer, P ),
    implies( P0, P, Strength, Prob0, Prob1 ),               % A 'soft implementation' rule
    modify( Prob1, Rules, Trace, Prob, RulesAnswers ).


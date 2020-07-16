/* Top-level driving procedure

Usage: expert.

*/

expert :-
    getquestion( Question ),                      % Input user's question
    ( answeryes( Question );                      % Try to find positive answer
      answerno( Question ) ).                     % If no positive answer then find negative


answeryes( Question ) :-                          % Look for positive answers to Question
    markstatus( negative),                        % No positive answer yet
    explore( Question, [], Answer ),              % Trace is empty
    positive( Answer ),                           % Look for positive answers
    markstatus( positive ),                       % Positive answer found
    present( Answer ), nl,
    write( 'More solutions? ' ),
    getreply( Reply ),                            % Read user's reply
    Reply = no.                                   % Otherwise backtrack to 'explore'


answerno( Question ) :-                           % Look for negative answer to Question
    retract( no_positive_answer_yet ), !,         % Has there been no positive answer?
    explore( Question, [], Answer),
    negative( Answer ),
    present( Answer ), nl,
    write( 'More negative solutions? ' ),
    getreply( Reply ),
    Reply = no.                                   % Otherwise backtrack to 'explore'


markstatus( negative ) :-
    assert( no_positive_answer_yet ).

markstatus( positive ) :-
    retract( no_positive_answer_yet ), !; true.


getquestion( Question ) :-
    nl, write( 'Question, please' ), nl,
    read( Question ).



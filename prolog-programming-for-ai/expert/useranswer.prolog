/* Procedure

useranswer( Goal, Trace, Answer )

generates, through backtracking, user-supplied solutions to Goal.
Trace is a chain of ancestor goals ans rules used for 'why' explanation.

*/

% Duplicate if 'explore' loaded
:- op( 900, xfx, : ).
:- op( 800, xfx, was ).
:- op( 870, fx, if ).
:- op( 880, xfx, then ).
:- op( 550, xfy, or ).
:- op( 540, xfy, and ).
:- op( 300, fx, 'derived by' ).
:- op( 600, xfx, from ).
:- op( 600, xfx, by ).

useranswer( Goal, Trace, Answer ) :-
    askable( Goal, _ ),                           % May be asked of the user
    freshcopy( Goal, Copy ),                      % Variables in Goal renamed
    useranswer( Goal, Copy, Trace, Answer, 1 ).

% Do not ask again about an instantiated goal

useranswer( Goal, _, _, _, N ) :-
    N > 1,                                        % Repeated question?
    instantiated( Goal ), !,
    fail.                                         % Do not ask again

% Is Goal implied true or false for all instantiations?

useranswer( Goal, Copy, _, Answer, _ ) :-
    wastold( Copy, Answer, _ ),
    instance_of( Copy, Goal ), !.                 % Answer to Goal implied

% Retrieve known solutions, indexed from N on, for Goal

useranswer( Goal, _, _, true, N ) :-
    wastold( Goal, true, M ),
    M >= N.

% Has everything already been said about Goal?

useranswer( Goal, Copy, _, Answer, _ ) :-
    end_answers( Copy ),
    instance_of( Copy, Goal ), !,                 % Everything was already said about Goal
    fail.

% Ask the user for (more) solutions

useranswer( Goal, _, Trace, Answer, N ) :-
    askuser( Goal, Trace, Answer, N ).


askuser( Goal, Trace, Answer, N ) :-
    askable( Goal, ExternFormat ),
    format( Goal, ExternFormat, Question, [], Variables ),  % Get question format
    ask( Goal, Question, Variables, Trace, Answer, N ).


ask( Goal, Question, Variables, Trace, Answer, N ) :-
    nl,
    ( Variables = [], !,                          % Introduce question
      write( 'Is it true: ' );
      write( 'Any (more) solutions to: ' ) ),
    write( Question ), write( '? ' ),
    getreply( Reply ), !,                         % Reply = yes/no/why
    process( Reply, Goal, Question, Variables, Trace, Answer, N ).


process( why, Goal, Question, Variables, Trace, Answer, N ) :-
    showtrace( Trace ),
    ask( Goal, Question, Variables, Trace, Answer, N ).

process( yes, Goal, _, Variables, Trace, true, N ) :-
    nextindex( Next ),                            % Get new free index for 'wastold'
    Next1 is Next + 1,
    ( askvars( Variables ),
      assertz( wastold( Goal, true, Next ) );     % Record solution
      freshcopy( Goal, Copy ),                    % Copy of Goal
      useranswer( Goal, Copy, Trace, Answer, Next1 ) ).  % More answers?

process( no, Goal, _, _, _, false, N ) :-
    freshcopy( Goal, Copy ),
    wastold( Copy, true, _ ), !,                  % 'no' mean: no more solutions
    assertz( end_answers( Goal ) ),               % Mark end of answers
    fail;
    nextindex( Next ),                            % Next free index for 'wastold'
    assertz( wastold( Goal, false, Next ) ).      % 'no' means: no solution


format( Var, Name, Name, Vars, [ Var/Name | Vars ] ) :-
    var( Var ), !.

foramt( Atom, Name, Atom, Vars, Vars ) :-
    atomic( Atom ), !,
    atomic( Name ).

format( Goal, Form, Question, Vars0, Vars ) :-
    Goal =.. [ Functor | Args1 ],
    Form =.. [ Functor | Forms ],
    formatall( Args1, Forms, Args2, Vars0, Vars ),
    Question =.. [ Functor | Args2 ].


formatall( [], [], [], Vars, Vars ).

formatall( [ X | XL ], [ F | FL ], [ Q | QL ], Vars0, Vars ) :-
    formatall( XL, FL, QL, Vars0, Vars1 ),
    format( X, F, Q, Vars1, Vars ).


askvars( [] ).

askvars( [ Variable/Name | Variables ] ) :-
    nl, write( Name ), write( ' = ' ),
    read( Variable ),
    askvars( Variables ).


showtrace( [] ) :-
    nl, write( 'This was your question' ), nl.

showtrace( [ Goal by Rule | Trace ] ) :-
    nl, write( 'To investigate, by ' ),
    write( Rule ), write( ', ' ),
    write( Goal ),
    showtrace( Trace ).


% instance_of( T1, T2 ) means instance of T1 is T2; that is,
% term T1 is more general than T2 or equally general as T2

instance_of( Term, Term1 ) :-                     % Instance of Term is Term1
    freshcopy( Term1, Term2 ),                    % Copy of Term1 with fresh set of variables
    numbervars( Term2, 0, _ ), !,
    Term = Term2.                                 % This succeeds if Term1 is instance of Term


freshcopy( Term, FreshTerm ) :-                   % Make a copy of Term with variables renamed
    asserta( copy( Term ) ),
    retract( copy( FreshTerm ) ), !.


% Index for 'wastold' at start

lastindex( 0 ).


% Next free index for 'wastold'

nextindex( Next ) :-
    retract( lastindex( Last ) ), !,
    Next is Last + 1,
    assert( lastindex( Next ) ).


% Displaying the conclusion of a consultation and 'how' explanation

present( Answer ) :- 
    nl, showconclusion( Answer ),
    nl, write( 'Would hou like to see how? ' ),
    getreply( Reply ),
    ( Reply = yes, !, show( Answer );             % Show solution tree
      true ).


showconclusion( Answer1 and Answer2 ) :- !,
    showconclusion( Answer1 ), write( ' and ' ),
    showconclusion( Answer2 ).

showconclusion( Conclusion was Found ) :-
    write( Conclusion ).


% 'show' displays a complete solution tree

show( Solution ) :-
    nl, show( Solution, 0 ), !.                   % Indent by 0

show( Answer1 and Answer2, H ) :- !,              % Indent by H
    show( Answer1, H ),
    tab( H ), write( and ), nl, 
    show( Answer2, H ).

show( Answer was Found, H ) :-                    % Indent by H
    tab( H ), writeans( Answer ),                 % Show conclusion
    nl, tab( H ),
    write( ' was ' ),
    show1( Found, H ).                            % Show evidence


show1( Derived from Answer, H ) :- !,
    write( Derived ), write( ' from ' ),          % Show rule name
    nl, H1 is H + 4,
    show( Answer, H1 ).                           % Show antecedent

show1( Found, _ ) :-                              % Found = 'told' or 'found as fact'
    write( Found ), nl.


writeans( Goal is true ) :- !,
    write( Goal ).                                % Omit 'is true' on output

writeans( Answer ) :-
    write( Answer ).

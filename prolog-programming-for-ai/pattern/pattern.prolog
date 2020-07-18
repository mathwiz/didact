/* A small interpreter for pattern-directed programs

The system's database is manipulated through assert/retract

Usage: run.

*/

:- op( 800, xfx, ---> ).


run :-
    Condition ---> Action,                        % A rule
    test( Condition ),                            % Precondition satisfied?
    execute( Action ).


test( [] ).                                       % Empty condition

test( [ First | Rest ] ) :-                       % Test conjunctive condition
    call( First ),
    test( Rest ).


execute( [ stop ] ) :- !.                         % Stop execution

execute( [] ) :-                                  % Empty action (execution cycle completed)
    run.                                          % Continue with next execution cycle

execute( [ First | Rest ] ) :-
    call( First ),
    execute( Rest ).


replace( A, B ) :-                                % Replace A with B in database
    retract( A ), !,
    assert( B ).


% Production rules for finding greatest common divisor (Euclid algorithm)

:- op( 300, fx, num ).


[ num X, num Y, X > Y ] --->
[ NewX is X - Y, replace( num X, num NewX ) ].

[ num X ] ---> [ write( X ), stop ].


% An initial database

num 25.

num 10.

num 15.

num 30.


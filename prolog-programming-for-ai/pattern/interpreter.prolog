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



/* A miniature implementation of Advice Language 0

This program plays a game from a given starting position using knowledge
represented in Advice Language 0

*/

:- op( 200, xfy, : ).
:- op( 220, xfy, .. ).
:- op( 185, fx, if ).
:- op( 190, xfx, then ).
:- op( 180, xfy, or ).
:- op( 160, xfy, and ).
:- op( 140, fx, not).


% Play the game starting at Pos
playgame( Pos ) :-
    playgame( Pos, nil ).                         % Start with empty forcing-tree

playgame( Pos, ForcingTree ) :-
    show( Pos ),
    ( end_of_game( Pos ),                         % End of game?
      write( 'End of game' ), nl, !;
      playmove( Pos, ForcingTree, Pos1, ForcingTree1 ), !,
      playgame( Pos1, ForcingTree1 ) ).


% Play 'us' move according to forcing-tree
playmove( Pos, Move .. FTree1, Pos1, FTree1 ) :-
    side( Pos, w ),                               % White = 'us'
    legalmove( Pos, Move, Pos1 ),
    showmove( Move ).

% Read 'them' move
playmove( Pos, FTree, Pos1, FTree1 ) :-
    side( Pos, b ),
    write( 'Your move: ' ),
    read( Move ),
    ( legalmove( Pos, Move, Pos1 ),
      subtree( FTree, Move, FTree1 ), !;          % Move down forcing-tree
      write( 'Illegal move' ), nl,
      playmove( Pos, FTree, Pos1, FTree1 ) ).

% If current forcing-tree is empty, generate a new one
playmove( Pos, nil, Pos1, FTree1 ) :-
    side( Pos, w ),
    resetdepth( Pos, Pos0 ),                      % Pos0 = Pos with depth 0
    strategy( Pos0, FTree ), !,                   % Generate new forcing-tree
    playmove( Pos0, FTree, Pos1, FTree1 ).


% Select a forcing subtree corresponding to Move
subtree( FTrees, Move, FTree ) :-
    member( Move .. FTree, FTrees ), !.

subtree( _, _, nil ).


% Find forcing tree for Pos
strategy( Pos, ForcingTree ) :-
    Rule : if Condition then AdviceList,          % Consult advice table
    holds( Condition, Pos, _ ), !,                % Match Pos against precondition
    member( AdviceName, AdviceList ),             % Try pieces of advice in turn
    nl, write( 'Trying' ), write( AdviceName ),
    satisfiable( AdviceName, Pos, ForcingTree ), !. % Satisfy AdviceName in Pos


satisfiable( AdviceName, Pos, FTree ) :-
    advice( AdviceName, Advice ),                 % Retrieve piece of advice
    sat( Advice, Pos, Pos, FTree ).               % 'sat' needs two positions for
                                                  % comparison predicates

sat( Advice, Pos, RootPos, FTree ) :-
    holdinggoal( Advice, HG ),
    holds( HG, Pos, RootPos ),                    % Holding goal satisfied
    sat1( Advice, Pos, RootPos, FTree ).


sat1( Advice, Pos, RootPos, nil ) :-
    bettergoal( Advice, BG ),
    holds( BG, Pos, RootPos ), !.                 % Better goal satisfied

sat1( Advice, Pos, RootPos, Move .. FTrees ) :-
    side( Pos, w ), !,                            % White = 'us'
    usmoveconstr( Advice, UMC ),
    move( UMC, Pos, Move, Pos1 ),                 % A move satisfying move-constr
    sat( Advice, Pos1, RootPos, FTrees ).

sat1( Advice, Pos, RootPos, FTrees ) :-
    side( Pos, b ), !,                            % Black = 'them'
    themmoveconstr( Advice, TMC ),
    bagof( Move .. Pos1, move( TMC, Pos, Move, Pos1 ), MPlist ),
    satall( Advice, MPlist, RootPos, FTrees ).    % Satisfiable in all successors


satall( _, [], _, [] ).

satall( Advice, [ Move .. Pos | MPlist ], RootPos, [ Move .. FT | MFTs ] ) :-
    sat( Advice, Pos, RootPos, FT ),
    satall( Advice, MPlist, RootPos, MFTs ).


% Interpreting holding and better-goals:
% A goal is an AND/OR/NOT combination of predicate names
holds( Goal1 and Goal2, Pos, RootPos ) :- !,
    holds( Goal1, Pos, RootPos ),
    holds( Goal2, Pos, RootPos ).

holds( Goal1 or Goal2, Pos, RootPos ) :- !,
    ( holds( Goal1, Pos, RootPos );
      holds( Goal2, Pos, RootPos ) ).

holds( not Goal, Pos, RootPos ) :- !,
    not holds( Goal, Pos, RootPos ).

holds( Pred, Pos, RootPos ) :-
    ( Cond =.. [ Pred, Pos ];                     % Most predicates do not depend on RootPos
      Cond =.. [ Pred, Pos, RootPos ] ),
    call( Cond ).


% Interpreting move constraints
move( MC1 and MC2, Pos, Move, Pos1 ) :- !,
    move( MC1, Pos, Move, Pos1 ),
    move( MC2, Pos, Move, Pos1 ).

move( MC1 then MC2, Pos, Move, Pos1 ) :- !,
    (move( MC1, Pos, Move, Pos1 );
     move( MC2, Pos, Move, Pos1 ) ).


% Selectors for components of piece of advice

bettergoal( BG : _, BG ).

holdinggoal( BG : HG : _, HG ).

usmoveconstr( BG : HG : UMC : _, UMC ).

themmoveconstr( BG : HG : UMC : TMC, TMC ).

member( X, [ X | L ] ).

member( X, [ Y | L ] ) :-
    member( X, L ).

% King and rook vs king in Advice Language 0

% Rules

/* don't know why this fails to compile
edge_rule : if their_king_edge and kings_close
            then [ mate_in_2, squeeze, approach, keeproom, divide_in_2, divide_in_3 ].


else_rule : if true
            then [ squeeze, approach, keeproom, divide_in_2, divide_in_3 ].
*/

% Pieces of advice

advice( mate_in_2,
        mate :
        not rooklost and their_king_edge :
        (depth = 0) and legal then (depth = 2) and checkmove :
        (depth = 1) and legal ).

advice( squeeze,
        newroomsmaller and not rookexposed and
        rookdivides and not stalemate :
        not rooklost :
        (depth = 0) and rookmove :
        nomove ).

advice( approach,
        okapproachedcsquare and not rookexposed and
        (rookdivides or lpatt) and (roomgt2 or not our_king_edge) :
        not rooklost :
        (depth = 0) and kingdiagfirst :
        nomove ).

advice( keeproom,
        themtomove and not rookexposed and rookdivides and okorndle and
        (roomgt2 or not okedge) :
        not rooklost :
        (depth = 0) and kingdiagfirst :
        nomove ).

advice( divide_in_2,
        themtomove and rookdivides and not rookexposed :
        not rooklost :
        (depth < 3) and legal :
        (depth < 2) and legal ).

advice( divide_in_3,
        themtomove and rookdivides and not rookexposed :
        not rooklost :
        (depth < 5) and legal :
        (depth < 4) and legal ).

/* Predicate library for king and rook vs king

Position is represented by: Side..Wx : Wy..Rx : Ry..Bx : By..Depth
Side is side to move ('w' or 'b')
Wx, Wy are x and y coordinates of White King
Rx, Ry are x and y coordinates of White Rook
Bx, By are x and y coordinates of Black King
Depth is depth of position in search tree

*/

% Selector relations

side( Side.._, Side ).
wk( _..WK.._, WK ).
wr( _.._..WR.._, WR ).
bk( _.._.._..BK.._, BK ).
depth( _.._.._.._..Depth, Depth ).

resetdepth( S..W..R..B..D, S..W..R..B..0 ).       % Copy of position with depth 0


% Some relations between squares

% Neighbor itegers 'within board'
n( N, N1 ) :-
    ( N1 is N + 1;
      N1 is N - 1 ),
    in( N1 ).


in( N ) :-
    N > 0, N < 9.


% Diagonal neighbotr squares
diagngb( X : Y, X1 : Y1 ) :-
    n( X, X1 ), n( Y, Y1 ).


% Vertical neighbor squares
verngb( X : Y, X : Y1 ) :-
    n( Y, Y1 ).


% Horizontal neighbor squares
horngb( X : Y, X1 : Y ) :-
    n( X, X1 ).


% Neighbor squares, first diagonal
ngb( S, S1 ) :-
    diagngb( S, S1 );
    horngb( S, S1 );
    verngb( S, S1 ).


end_of_game( Pos ) :-
    mate( Pos ).


% Move constraints predicates
% These are specialized move generators:
% move( MoveConstr, Pos, Move, NewPos )

move( depth < Max, Pos, Move, Pos1 ) :-
    depth( Pos, D ),
    D < Max, !.

move( depth = D, Pos, Move, Pos1 ) :-
    depth( Pos, D ), !.

move( kingdiagfirst, w..W..R..B..D, W-W1, b..W1..R..B..D1 ) :-
    D1 is D + 1,
    ngb( W, W1 ),                                 % 'ngb' generates diagonal moves first
    not ngb( W1, B ),                             % Must not move into check
    W1 \== R.                                     % Must not collide with rook

move( rookmove, w..W..Rx : Ry..B..D, Rx : Ry-R, b..W..R..B..D1 ) :-
    D1 is D + 1,
    coord( I ),                                   % Integer between 1 and 8
    ( R = Rx : I; R = I : Ry ),                   % Move vertically or horizontally
    R \== Rx : Ry,                                % Must have moved
    not inway( Rx : Ry, W, R ).                   % White king not in way

move( checkmove, Pos, R-Rx : Ry, Pos1 ) :-
    wr( Pos, R ),
    bk( Pos, Bx : By ),
    ( Rx = Bx; Ry = By ),                         % Rook and Black king in line
    move( rookmove, Pos, R-Rx : Ry, Pos1 ).

move( legal, w..P, M, P1 ) :-
    ( MC = kingdiagfirst; MC = rookmove ),
    move( MC, w..P, M, P1 ).

move( legal, b..W..R..B..D, B-B1, w..W..R..B1..D1 ) :-
    D1 is D + 1,
    ngb( B, B1 ),
    not check( w..W..R..B1..D1 ).


legalmove( Pos, Move, Pos1 ) :-
    move( legal, Pos, Move, Pos1 ).


check( _..W..Rx : Ry..Bx : By.._ ) :-
    ngb( W, Bx : By );                            % King's too close
    ( Rx = Bx; Ry = By ),
    Rx : Ry \== Bx : By,                          % Not rook captured
    not inway( Rx : Ry, W, Bx : By ).


inway( S, S1, S1 ) :- !.

inway( X1, Y, X2 : Y, X3 : Y ) :-
    ordered( X1, X2, X3 ), !.

inway( X : Y1, X : Y2, X : Y3 ) :-
    ordered( Y1, Y2, Y3 ).


ordered( N1, N2, N3 ) :-
    N1 < N2, N2 < N3;
    N3 < N2, N2 < N1.


coord(1). coord(2). coord(3). coord(4).
coord(5). coord(6). coord(7). coord(8).


% Goal predicates

true( Pos ).


themtomove( b.._ ).                               % Black = 'them' to move


mate( Pos ) :-
    side( Pos, b ),
    check( Pos ),
    not legalmove( Pos, _, _ ).


stalemate( Pos ) :-
    side( Pos, b),
    not check( Pos ),
    not legalmove( Pos, _, _ ).


newroomsmaller( Pos, RootPos ) :-
    room( Pos, Room ),
    room( RootPos, RootRoom ),
    Room < RootRoom.


rookexposed( Side..W..R..B.._ ) :-
    dist( W, R, D1 ),
    dist( B, R, D2 ),
    ( Side = w, !, D1 > D2 + 1;
      Side = b, !, D1 > D2 ).


okapproachedcsquare( Pos, RootPos ) :-
    okcsquaremdist( Pos, D1 ),
    okcsquaremdist( RootPos, D2 ),
    D1 < D2.


okcsquaremdist( Pos, Mdist ) :-           % Manhattan dist between WK and critical square
    wk( Pos, WK ),
    cs( Pos, CS ),                        % Critial square
    manhdist( WK, CS, Mdist ).


rookdivides( _..Wx : Wy..Rx : Ry..Bx : By.._ ) :-
    ordered( Wx, Rx, Bx ), !;
    ordered( Wy, Ry, By ).


lpatt( _..W..R..B.._ ) :-                         % L-pattern
    manhdist( W, B, 2 ),
    manhdist( R, B, 3 ).


okorndle( _..W..R.._, _..W1..R1.._ ) :-
    dist( W, R, D ),
    dist( W1, R1, D1 ),
    D =< D1.


roomgt2( Pos ) :-
    room( Pos, Room ),
    Room > 2.


our_king_edge( _..X : Y.._ ) :-                   % White king on edge
    ( X = 1, !; X = 8, !; Y = 1, !; Y = 8 ).


their_king_edge( _..W..R..X : Y.._ ) :-           % Black king on edge
    ( X = 1, !; X = 8, !; Y = 1, !; Y = 8 ).


kings_close( Pos ) :-                             % Distance between kings < 4
    wk( Pos, WK), bk( Pos, BK ),
    dist( WK, BK, D ),
    D < 4.


rooklost( _..W..B..B.._ ).                        % Rook has been captured

rooklost( b..W..R..B.._ ) :-
    ngb( B, R ),                                  % Black king attacks rook
    not ngb( W, R ).                              % White king does not defend


dist( X : Y, X1 : Y1, D ) :-                      % Distance in king moves
    absdiff( X, X1, Dx ),
    absdiff( Y, Y1, Dy ),
    max( Dx, Dy, D ).


absdiff( A, B, D ) :-
    A > B, !, D is A-B;
    D is B-A.


max( A, B, M ) :-
    A >= B, !, M = A;
    M = B.


% Manhattan distance
manhdist( X : Y, X1 : Y1, D ) :-
    absdiff( X, X1, Dx ),
    absdiff( Y, Y1, Dy ),
    D is Dx + Dy.


% Area to which Black king is confined
room( Pos, Room ) :-
    wr( Pos, Rx : Ry ),
    bk( Pos, Bx : By ),
    ( Bx < Rx, SideX is Rx - 1; Bx > Rx, SideX is 8 - Rx ),
    ( By < Ry, SideY is Ry - 1; By > Ry, SideY is 8 - Ry ),
    Room is SideX * SideY, !;
    Room is 64.                                   % Rook in line with Black king


% 'Critical square'
cs( _..W..Rx : Ry..Bx : By.._, Cx : Cy ) :-
    ( Bx < Rx, !, Cx is Rx - 1; Cx is Rx + 1),
    ( By < Ry, !, Cy is Ry - 1; Cy is Ry + 1).


% Display procedures

show( Pos ) :-
    nl,
    coord( Y ), nl,
    coord( X ),
    writepiece( X : Y, Pos ),
    fail.

show( Pos ) :-
    side( Pos, S ), depth( Pos, D ),
    nl, write( 'Side= ' ), write( S ),
    write( 'Depth= ' ), write( D ), nl.


writepiece( Square, Pos ) :-
    wk( Pos, Square ), !, write( 'W' );
    wr( Pos, Square ), !, write( 'R' );
    bk( Pos, Square ), !, write( 'B' );
    write( '.' ).


showmore( Move ) :-
    nl, write( Move ), nl.


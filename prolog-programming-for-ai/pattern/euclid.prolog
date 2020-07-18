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


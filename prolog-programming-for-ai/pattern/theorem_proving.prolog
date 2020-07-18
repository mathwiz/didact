% Production rules for resolution theorem proving

% Contradicting clauses

[ clause( X ), clause( ~X ) ] --->
[ write( 'Contradiction found' ), stop ].


% member( a, [b,c,a] ).


conc( [X | L1], L2, [X, L3] ) :-
    conc( L1, L2, L3 ).

% conc( [a,b], [c,d], [a,b,a,c,d] ).
% conc( [a,[b,c],d], [a,[],b], L).
% conc( L1, L2, [a,b,c] ). 
% conc( Before, [may | After], [jan,feb,mar,apr,may,jun,jul,aug,sep,oct,nov,dec] ).


add( X, L, [X | L] ).


% delete( a, [a,b,a,a], L ).
% delete( a, L, [1,2,3] ).


% sublist( [c,d,e], [a,b,c,d,e,f] ).


% permutation( [a,b,c], P ).



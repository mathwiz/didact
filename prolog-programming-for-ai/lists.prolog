% member( a, [b,c,a] ).


conc( [X | L1], L2, [X, L3] ) :-
    conc( L1, L2, L3 ).

% conc( [a,b], [c,d], [a,b,a,c,d] ).
% conc( [a,[b,c],d], [a,[],b], L).
% conc( L1, L2, [a,b,c] ). 
% conc( Before, [may | After], [jan,feb,mar,apr,may,jun,jul,aug,sep,oct,nov,dec] ).


add( X, L, [X | L] ).


% delete( [a,b,a,a], a, L ).
% delete( [1,2,3], 1, L ).


% sublist( [c,d,e], [a,b,c,d,e,f] ).
% sublist( L, [a,b,c] ).

% permutation( [a,b,c], P ).
% permutation( [1,1,0,1], P).
% permutation( [1,2,3,4], P).


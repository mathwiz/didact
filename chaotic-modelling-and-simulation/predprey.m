% predprey - generate vectors of population sizes for 2 interacting species
% exmaple: predprey(0.04, 0.0004, 0.08, 0.0002, 500, 120, 600);
function [T A B] = predprey(a, b, c, d, A0, B0, n)
	% T vector of time
	% A prey population
	% B predator population
	% n number of time intervals
	% a natural growth rate of A
	% b death rate from predation
	% c natural mortality rate of B
	% d growth rate from eating prey
	T = 1:n;
	A = zeros(1,n);
	B = zeros(1,n);
	A(1) = A0;
	B(1) = B0;
	for k=2:n
		A(k) = A(k-1) + a*A(k-1) - b*A(k-1)*B(k-1);
		B(k) = B(k-1) - c*B(k-1) + d*A(k-1)*B(k-1);
	end;
endfunction
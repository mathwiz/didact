% Approx of Cauchy problem:
%        y'(t)=f(t, y(t)) for t in [t0, tf]
%        y(t0)=y0
%
% Input:
% f = function handle for f(t, y)
% t0, tf = endpoints of time
% y0 = initial condition
% Nh = number of points of discretisation (excluding t0)
% 
% Output:
% tn = column vector of discretisation points (including t0)
% un = column vector of numerical solution (including u0=y0)
%
function [tn,un]=euler(f,t0,tf,y0,Nh)  
% discretisation  points
tn=linspace(t0,tf,Nh+1)';
% discretisation step
h=(tf-t0)/Nh;
% initialize vector of solution
un=zeros(Nh+1, 1);
% store intial condition
un(1)=y0;
% use Euler method
for n=1:Nh
  un(n+1)=un(n)+h*f(tn(n),un(n));
end

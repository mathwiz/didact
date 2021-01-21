% Euler method of 2 equation Cauchy problem:
%        p'(t)=f1(t, p(t), s(t)) for t in [t0, tf]
%        s'(t)=f2(t, p(t), s(t)) for t in [t0, tf]
%        p(t0)=p0
%        s(t0)=s0
%
% Input:
% f1 = function handle for p
% f2 = function handle for s
% t0, tf = endpoints of time
% p0, s0 = initial condition
% Nh = number of points of discretisation (excluding t0)
% 
% Output:
% tn = column vector of discretisation points (including t0)
% un, vn = column vectors of numerical solutions (including u0=p0, v0=s0)
%
function [tn,un,vn]=euler2(f1,f2,t0,tf,p0,s0,Nh)
% discretisation  points
tn=linspace(t0,tf,Nh+1)';
% discretisation step
h=(tf-t0)/Nh;
% initialize vector of solution
un=zeros(Nh+1, 1);
vn=zeros(Nh+1, 1);
% store intial condition
un(1)=p0;
vn(1)=s0;
% use Euler method
for n=1:Nh
  un(n+1)=un(n)+h*f1(tn(n),un(n),vn(n));
  vn(n+1)=un(n)+h*f2(tn(n),un(n),vn(n));
end

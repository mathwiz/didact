% Example: cauchy_problem(1000)
% Input: number of time steps
%
function cauchy_problem(Nh)
% endpoints of time
t0=0; tf=10;
% initial condition
y0=0;
% function f(t,y)
f=@(t,y) (cos(t)-y) ./ (t+1);
[tn,un]=euler(f,t0,tf,y0,Nh);
% plot the numerical solution
figure(1);
plot(tn,un,'.','Markersize',16); grid on; hold on; 
xlabel('t'); ylabel('y(t)');

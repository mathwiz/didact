% Example: dolphins3(100)
% Input: Nh = number of time steps
% Output: un_final = solution at final time
%
function dolphins3(Nh)
% endpoints of time
t0=0; tf=10;
% initial condition
y0=88;
% functions
b=@(t) -0.03 * (2 - exp(-5*(sin(t*pi)).^2));
f=@(t,y) 0.16 * (1 - y/150) * y + b(t) * y;
[tn,un]=euler(f,t0,tf,y0,Nh);
un_final=un(end);
% plot the numerical solution
figure(1);
plot(tn,un,'.','Markersize',16); grid on; hold on; 
xlabel('t'); ylabel('y(t)');

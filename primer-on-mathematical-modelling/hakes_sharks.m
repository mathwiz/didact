% Example: hakes_sharks(1000)
% Input: Nh = number of time steps
%
function hakes_sharks(Nh)
% endpoints of time
t0=0; tf=10;
% initial condition
p0=5000; s0=40;
% parameters of model
rp=0.6; rs=-0.2; 
Kp=3.e5; 
sigmap=rp/Kp;
sigmas=1.e-4;
alpha=0.001;
mu=0.01;
% functions
f1=@(t,p,s) (rp - sigmap*p - alpha*s) * p;
f2=@(t,p,s) (rs - sigmas*s + mu*alpha*p) * s;
[tn,un,vn]=euler2(f1,f2,t0,tf,p0,s0,Nh);
fprintf('%6.0f hakes and %6.0f sharks at t=%d \n',
        un(end), vn(end), tf);

% plot the numerical solution
figure(1); clf;
[ax,h1,h2] = plotyy(tn,un,tn,vn); grid on;
set(h1,'Linewidth',2);
set(h2,'Linewidth',2);

% plot phase plane
figure(2); clf;
plot(un(1),vn(1),'r*',un,vn,'Linewidth',2);
grid on;


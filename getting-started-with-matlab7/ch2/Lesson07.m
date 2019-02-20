%% Publishing Reports - Example
%% Simple Spiral Plot
% Let a spiral be given by
% r(t) = exp(-theta/10), 0<=theta<=10*pi)

%% Create vectors theta and r
theta = linspace(0, 10*pi, 200);
r = exp(-theta/10);

%% Plot theta vs r in polar
polar(theta, r)

%% For publishing
% publish('Lesson07.m', 'html')
% open html/Lesson07.html

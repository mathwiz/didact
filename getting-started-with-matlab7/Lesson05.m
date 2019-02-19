function Lesson05(r)
% Call example: Lesson05(4)

  display(['Call circlefn with r = ', num2str(r)])
  [x,y] = circlefn(r);
  display('Examine values of x and y')
endfunction

function [x, y] = circlefn(r)
% Call syntax: [x,y] = circlefn(r); or circlefn(r);
% Output: [x,y] = the x and y coordinates

theta = linspace(0, 2*pi, 100);
x = r * cos(theta);
y = r * sin(theta);

plot(x,y);
axis('equal');
title(['Circle of radius = ', num2str(r)]);
endfunction
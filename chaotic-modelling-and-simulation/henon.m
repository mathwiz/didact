% henon
% example: henon(600, -1, 1.4, 0.3)
function [ X Y ] = henon(n, x0, a, b)
  X = zeros(n, 1);
  Y = zeros(n, 1);
  X(1) = x0;
  Y(1) = b * X(1);
  for k = 1:(n-1)
    X(k+1) = Y(k) + (1 - a*X(k)^2);
    Y(k+1) = b * X(k);
  end;
endfunction

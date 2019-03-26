function [ X Y ] = lvdifference(n, x0, y0, rx, ry, a, b)
  % example: lvdifference(640, .1, .1, .2, .2, .02, .01);
  X = zeros(n, 1);
  Y = zeros(n, 1);
  X(1) = x0;
  Y(1) = y0;
  for k = 1:(n-1)
    X(k+1) = generatelv(X(k), Y(k), rx, a);
    Y(k+1) = generatelv(Y(k), X(k), ry, b);
  end;
endfunction

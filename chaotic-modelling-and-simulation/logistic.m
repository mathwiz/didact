function [ X ] = logistic(n, x0, b)
  X = zeros(n, 1);
  X(1) = x0;
  for k = 1:(n-1)
    X(k+1) = b * X(k) * (1-X(k));
  end;
endfunction

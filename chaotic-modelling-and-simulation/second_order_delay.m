% second_order_delay
% example: second_order_delay(600, .1, 1.37)
function [ X ] = second_order_delay(n, x0, b)
  X = zeros(n, 1);
  X(3) = x0;
  for k = 3:(n-1)
    X(k+1) = -b*X(k) + (X(k) * X(k-2));
  end;
endfunction

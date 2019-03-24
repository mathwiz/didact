function [iterations] = plot_phase(X, Y)
  iterations = min(length(X), length(Y));
  xs = X(1:iterations);
  ys = Y(1:iterations);
  plot(xs, ys, 'k.', xs, ys, 'k');
endfunction
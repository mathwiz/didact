function [plots] = plot_time(X)
  hold on;
  for k = 1:columns(X)
    xs = (1:rows(X)) - 1;
    ys = X(:,k);
    plot(xs, ys, 'k.', xs, ys, 'k');
  end;
  hold off;
  plots = k;
endfunction
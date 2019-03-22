x0 = 0.67
bmin = 2.71
bmax = 3.99
iterations = 256
spacing = 297
plotpoints = 99

hold on;

bs = linspace(bmin, bmax, spacing);
plotstartindex = max(1, iterations-plotpoints);

for k = bs
	xs = repmat(k, 1, iterations);
	ys = logisticapply(x0, k, iterations);
	plot(xs(plotstartindex:end), ys(plotstartindex:end), 'k.');
end

hold off;
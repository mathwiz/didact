import math
import random
from collections import Counter
from matplotlib import pyplot as plt


def uniform_pdf(x):
	return 1 if x >= 0 and x < 1 else 0


def uniform_cdf(x):
	if x < 0: 
		return 0
	elif x < 1: 
		return x
	return 1


def normal_pdf(x, mu=0, sigma=1):
	sqrt_two_pi = math.sqrt(2 * math.pi)
	return (math.exp(-(x - mu) ** 2 / 2 / sigma ** 2) / (sqrt_two_pi * sigma))


def normal_cdf(x, mu=0, sigma=1):
	return (1 + math.erf((x - mu) / math.sqrt(2) / sigma)) / 2


def inverse_normal_cdf(p, mu=0, sigma=1, tolerance=0.00001):
	if mu != 0 or sigma != 1:
		return mu + sigma * inverse_normal_cdf(p, tolerance=tolerance)

	low_z = -10.0
	hi_z = 10.0
	while hi_z - low_z > tolerance:
		mid_z = (low_z + hi_z) / 2
		mid_p = normal_cdf(mid_z)
		if mid_p < p:
			low_z = mid_z
		elif mid_p > p:
			hi_z = mid_z
		else:
			break

	return mid_z


def bernoulli_trial(p):
	return 1 if random.random() < p else 0


def binomial(n, p):
	return sum(bernoulli_trial(p) for _ in range(n))


def make_hist(p, n, num_points):
	data = [binomial(n, p) for _ in range(num_points)]
	histogram = Counter(data)
	# barchart for histogram
	plt.bar([x - 0.4 for x in histogram.keys()],
			[v / num_points for v in histogram.values()],
			0.8,
			color='0.75')

	mu = p*n
	sigma = math.sqrt(n*p*(1 - p))
	# line chart for normal approx
	xs = range(min(data), max(data) + 1)
	ys = [normal_cdf(i + 0.5, mu, sigma) - normal_cdf(i - 0.5, mu, sigma) for i in xs]
	plt.plot(xs, ys)
	plt.title("Binomial Distribution vs. Normal Approximation")
	plt.show()
	return None


def normal_approximation_to_binomial(n, p):
	return p*n, math.sqrt(p*(1 - p)*n)


# function defined as alias to existing function
normal_probability_below = normal_cdf


def normal_probability_above(lo, mu=0, sigma=1):
	return 1 - normal_cdf(lo, mu, sigma)


def normal_probability_between(lo, hi, mu=0, sigma=1):
	return normal_cdf(hi, mu, sigma) - normal_cdf(lo, mu, sigma)


def normal_probability_outside(lo, hi, mu=0, sigma=1):
	return 1 - normal_probability_between(lo, hi, mu, sigma)


def normal_upper_bound(probability, mu=0, sigma=1):
	"""
	return the z for which P(Z <= z) = probability
	"""
	return inverse_normal_cdf(probability, mu, sigma)


def normal_lower_bound(probability, mu=0, sigma=1):
	"""
	return the z for which P(Z >= z) = probability
	"""
	return inverse_normal_cdf(1 - probability, mu, sigma)


def normal_two_sided_bounds(probability, mu=0, sigma=1):
	tail_probability = (1 - probability) / 2
	upper_bound = normal_lower_bound(tail_probability, mu, sigma)
	lower_bound = normal_upper_bound(tail_probability, mu, sigma)
	return lower_bound, upper_bound




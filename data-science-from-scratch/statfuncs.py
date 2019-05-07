import math
from collections import Counter
import vecmat

def mean(x):
	return sum(x) / len(x)


def median(v):
	n = len(v)
	sorted_v = sorted(v)
	midpoint = n // 2

	if n % 2 == 1:
		return sorted_v[midpoint]
	else:
		lo = midpoint - 1
		hi = midpoint
		return (sorted_v[lo] + sorted_v[hi]) / 2


def quantile(x, p):
	"""
	return the pth-percentile value in x
	"""
	p_index = int(p * len(x))
	return sorted(x)[p_index]


def mode(x):
	"""
	returns a list, might be more than one mode
	"""
	counts = Counter(x)
	max_count = max(counts.values())
	return [x_i for x_i, count in counts.items() if count == max_count]


def data_range(x):
	return max(x) - min(x)


def de_mean(x):
	"""
	translate x by subtracting its mean (so the result has mean 0)
	"""
	x_bar = mean(x)
	return [x_i - x_bar for x_i in x]


def variance(x):
	"""
	assumes x has n >= 2
	"""	
	n  = len(x)
	deviations = de_mean(x)
	return vecmat.sum_of_squares(deviations) / (n - 1)






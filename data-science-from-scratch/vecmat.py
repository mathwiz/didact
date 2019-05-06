import math


def vector_add(v, w):
	"""
	adds corresponding elements
	"""
	return [v_i + w_i for v_i, w_i in zip(v, w)]


def vector_subtract(v, w):
	"""
	subtracts corresponding elements
	"""
	return [v_i + w_i for v_i, w_i in zip(v, w)]


def vector_sum(vecs):
	return reduce(vector_add, vecs)


def scalar_multiply(c, v):
	return [c * v_i for v_i in v]


def vector_mean(vecs):
	"""
	compute the vector whose ith element is the mean of
	the ith elements of the input vectors
	"""
	n = len(vecs)
	return scalar_multiply(1/n, vector_sum(vecs))


def dot(v, w):
	return sum(v_i * w_i for v_i, w_i in zip(v, w))


def sum_of_squares(v):
	return dot(v, v)


def magnitude(v):
	return math.sqrt(sum_of_squares(v))


def squared_distance(v, w):
	return sum_of_squares(vector_subtract(v, w))


def distance(v, w):
	return math.sqrt(squared_distance(v, w))


def shape(A):
	rows = len(A)
	cols = len(A[0]) if A else 0
	return rows, cols


def get_row(A, i):
	return A[i]


def get_column(A, j):
	return [A_i[j] for A_i in A]


def make_matrix(rows, cols, entry_fn):
	"""
	returns a rows x cols make_matrix
	whose (i, j)th entry is entry_fn(i, j)
	"""
	return [[entry_fn(i, j) for j in range(cols)] for i in range(rows)]


def is_diagonal(i, j):
	return 1 if i == j else 0





import math
import unittest
import random
import numpy as np
from gradient_descent import *
from vecmat import *


class MyTestCase(unittest.TestCase):

    def test_diff_quotient(self):
        f = lambda x : x ** 2
        res = difference_quotient(f, 3, .01)
        np.testing.assert_almost_equal(res, 6.00, 2)


    def test_gradient(self):
        f = lambda xs : [x ** 2 for x in xs]
        vec = [random.randint(-10,10) for i in range(3)]
        tolerance = 0.0000001
        while True:
            gradient = sum_of_squares_gradient(vec)
            next_v = step(vec, gradient, -0.01)
            if distance(next_v, vec) < tolerance:
                break
            vec = next_v
        print(vec)



if __name__ == '__main__':
    unittest.main()

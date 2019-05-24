import math
import unittest
import random
import numpy as np
import gradient_descent as funcs


class MyTestCase(unittest.TestCase):

    def test_diff_quotient(self):
        f = lambda x : x ** 2
        res = funcs.difference_quotient(f, 3, .01)
        np.testing.assert_almost_equal(res, 6.00, 2)




if __name__ == '__main__':
    unittest.main()

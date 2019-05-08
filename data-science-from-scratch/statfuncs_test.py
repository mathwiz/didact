import unittest
import math
import statfuncs as stats
import numpy as np


v = [1,2,3]
w = [1,2,3,1]
x = [1,2,3,1,2]

class MyTestCase(unittest.TestCase):
    def test1(self):
        self.assertEqual(stats.median(v), 2)
        self.assertEqual(stats.median(w), 1.5)

    def test2(self):
        self.assertEqual(stats.mode(v), [1,2,3])
        self.assertEqual(stats.mode(w), [1])
        self.assertEqual(stats.mode(x), [1,2])

    def test3(self):
        self.assertEqual(stats.data_range(w), 2)

    def test4(self):
        np.testing.assert_almost_equal(stats.variance(w), .916, 3)

    def test5(self):
        self.assertEqual(stats.quantile(w, 0.25), 1)
        self.assertEqual(stats.quantile(w, 0.75), 3)
        self.assertEqual(stats.interquartile_range(w), 2)

    def test6(self):
        self.assertEqual(stats.covariance([1,1,1,1,1], x), 0)
        self.assertEqual(stats.covariance([4,5,6,0,0], x), 1.25)

    def test7(self):
        np.testing.assert_almost_equal(stats.correlation([4,5,6,0,0], x), 0.528, 3)
        np.testing.assert_almost_equal(stats.correlation([-2,-1,0,1,2], [2,1,0,1,2]), 0.0, 3)



if __name__ == '__main__':
    unittest.main()
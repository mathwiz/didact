import unittest
import numpy as np
import probability as funcs


class MyTestCase(unittest.TestCase):
    def test1(self):
        self.assertEqual(funcs.normal_approximation_to_binomial(100, .5), (50.0, 5.0))


    def test2(self):
        np.testing.assert_almost_equal(funcs.normal_cdf(1.69), .95, 2)
        np.testing.assert_almost_equal(funcs.normal_cdf(-1.69), .05, 2)
        np.testing.assert_almost_equal(funcs.normal_probability_below(1.69), .95, 2)
        np.testing.assert_almost_equal(funcs.normal_probability_above(1.69), .05, 2)


    def test3(self):
        np.testing.assert_almost_equal(funcs.normal_probability_between(-1.96, 1.96), .95, 2)
        np.testing.assert_almost_equal(funcs.normal_probability_outside(-1.96, 1.96), .05, 2)


    def test4(self):
        np.testing.assert_almost_equal(funcs.normal_upper_bound(.95), 1.65, 2)
        np.testing.assert_almost_equal(funcs.normal_lower_bound(.95), -1.65, 2)


    def test5(self):
        lo_95, hi_95 = funcs.normal_two_sided_bounds(.95)
        np.testing.assert_almost_equal(lo_95, -1.96, 2)
        np.testing.assert_almost_equal(hi_95, 1.96, 2)
        lo_99, hi_99 = funcs.normal_two_sided_bounds(.99)
        np.testing.assert_almost_equal(lo_99, -2.58, 2)
        np.testing.assert_almost_equal(hi_99, 2.58, 2)


    # def test6(self):
    #     self.assertTrue(funcs.binomial(1000, .5) > 400)


if __name__ == '__main__':
    unittest.main()
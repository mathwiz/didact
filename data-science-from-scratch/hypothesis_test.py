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


    def test3(self):
        np.testing.assert_almost_equal(funcs.normal_probability_between(-1.96, 1.96), .95, 2)
        np.testing.assert_almost_equal(funcs.normal_probability_outside(-1.96, 1.96), .05, 2)


    # def test5(self):
    #     self.assertTrue(funcs.bernoulli_trial(.5) == 0 or funcs.bernoulli_trial(.5) == 1)

    # def test6(self):
    #     self.assertTrue(funcs.binomial(1000, .5) > 400)


if __name__ == '__main__':
    unittest.main()
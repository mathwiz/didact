import unittest
import probability as funcs


class MyTestCase(unittest.TestCase):
    def test1(self):
        print(funcs.normal_approximation_to_binomial(100, .5))


    # def test2(self):
    #     self.assertEqual(funcs.uniform_cdf(.5), .5)
    #     self.assertEqual(funcs.uniform_cdf(1.5), 1)
    #     self.assertEqual(funcs.uniform_cdf(0.0), 0)

    # def test3(self):
    #     np.testing.assert_almost_equal(funcs.normal_pdf(2), .06, 2)
    #     np.testing.assert_almost_equal(funcs.normal_pdf(0), .40, 2)
    #     np.testing.assert_almost_equal(funcs.normal_pdf(-2), .05, 2)

    # def test4(self):
    #     np.testing.assert_almost_equal(funcs.normal_cdf(2), .97, 2)
    #     np.testing.assert_almost_equal(funcs.normal_cdf(0), .5, 2)
    #     np.testing.assert_almost_equal(funcs.normal_cdf(-2), .03, 2)

    # def test5(self):
    #     self.assertTrue(funcs.bernoulli_trial(.5) == 0 or funcs.bernoulli_trial(.5) == 1)

    # def test6(self):
    #     self.assertTrue(funcs.binomial(1000, .5) > 400)


if __name__ == '__main__':
    unittest.main()
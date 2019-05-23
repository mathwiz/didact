import math
import unittest
import random
import numpy as np
import probability as funcs


def run_experiment(n=1000):
    return [random.random() < 0.5 for _ in range(n)]


def reject_fairness(experiment):
    mu, sigma = funcs.normal_approximation_to_binomial(len(experiment), 0.5)
    lower, upper = funcs.normal_two_sided_bounds(0.95, mu, sigma)
    num_heads = len([flip for flip in experiment if flip])
    return num_heads < lower or num_heads > upper



class MyTestCase(unittest.TestCase):

    def test_extreme(self):
        iterations = 10000
        extreme_value_count = 0
        for _ in range(iterations):
            num_heads = sum((1 if random.random() < 0.5 else 0) for _ in range(1000))
            if num_heads >= 530 or num_heads <= 470:
                extreme_value_count += 1

        res = (extreme_value_count / iterations)
        print("Extreme values pct:", res)
        self.assertTrue(res != 0)


    def test_confidence_interval(self):
        p_hat = 540 / 1000.0
        mu = p_hat
        sigma = math.sqrt(p_hat*(1 - p_hat) / 1000)
        lower, upper = funcs.normal_two_sided_bounds(0.95, mu, sigma)
        # this many heads implies unfair coin, CI does not include 0.5
        self.assertTrue(lower > .5)
        self.assertTrue(upper > .5)


    def test_p_hacking(self):
        iterations = 1000
        #random.seed(0)
        experiments = [run_experiment() for _ in range(iterations)]
        num_rejections = len([experiment
                                for experiment in experiments
                                if reject_fairness(experiment)])
        print("Hypothesis rejected in", num_rejections, "of", iterations, "experiments")
        self.assertTrue(num_rejections != 0)



if __name__ == '__main__':
    unittest.main()


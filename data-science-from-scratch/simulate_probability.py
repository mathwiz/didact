import unittest
import random
import numpy as np
import probability as funcs


class MyTestCase(unittest.TestCase):

    def test1(self):
        iterations = 10000
        extreme_value_count = 0
        for _ in range(iterations):
            num_heads = sum((1 if random.random() < 0.5 else 0) for _ in range(1000))
            if num_heads >= 530 or num_heads <= 470:
                extreme_value_count += 1

        res = (extreme_value_count / iterations)
        print(res)
        self.assertTrue(res != 0)
        

if __name__ == '__main__':
    unittest.main()


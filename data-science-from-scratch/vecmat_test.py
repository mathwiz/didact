import unittest
import math
import vecmat


v = [1,2,3]
w = [4,5,6]

class MyTestCase(unittest.TestCase):
    def test1(self):
        self.assertEqual(vecmat.dot(v,w), 32)

    def test2(self):
        self.assertEqual(vecmat.sum_of_squares(v), 14)


if __name__ == '__main__':
    unittest.main()
import unittest
import math
import statfuncs as stats


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


if __name__ == '__main__':
    unittest.main()
import unittest


class MySet:

    def __init__(self, values=None):
        self.dict = {}
        if values is not None:
            for value in values:
                self.add(value)

    def __repr__(self):
        return "MySet: " + str(self.dict.keys())

    def add(self, value):
        self.dict[value] = True

    def contains(self, value):
        return value in self.dict

    def remove(self, value):
        del self.dict[value]

    def size(self):
        return len(self.dict)


class MySetTest(unittest.TestCase):
    def test1(self):
        s = MySet([1,2,3])
        self.assertTrue(s.contains(2))
        self.assertEqual(s.size(), 3)
        s.add(4)
        self.assertEqual(s.size(), 4)
        s.remove(3)
        self.assertEqual(s.size(), 3)
        self.assertFalse(s.contains(3))
        


if __name__ == '__main__':
    unittest.main()

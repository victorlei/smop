import numpy as np
import unittest
from runtime import *
from sparsearray import sparsearray

class Slice(unittest.TestCase):
     def setUp(self):
         self.a = sparsearray(arange_(1,4).reshape(2,2,order="F"))

     def test01(self):
         #self.assertTrue(isequal_(self.a[1:4],[1,2,3,4]))
         self.assertEqual(self.a[1,1], 1)
         self.assertEqual(self.a[2,1], 2)
         self.assertEqual(self.a[1,2], 3)
         self.assertEqual(self.a[2,2], 4)

class Sparsearray(unittest.TestCase):
    def setUp(self):
        self.a = np.arange(1,5).reshape(2,2,order="F")
        self.b = sparsearray(self.a)

    def test01(self):
        self.assertEqual(self.b[1,1],1)
        self.assertEqual(self.b[2,1],2)
        self.assertEqual(self.b[1,2],3)
        self.assertEqual(self.b[2,2],4)

class Index(unittest.TestCase):
    def setUp(self):
        self.a = sparsearray(arange_(1,4).reshape(2,2,order="F"))

    def test01(self):
        """
        One-dimensional LHS indexing of multi-dimensional array,
        without expansion.  Using scalar index.
        """
        self.a[4]=9
        self.assertTrue(isequal_(self.a, sparsearray([[1, 3],[2, 9]])))

    def test02(self):
        """
        Multi-dimensional LHS indexing, without expansion.
        """
        self.a[1,1]=11
        self.a[2,2]=22
        self.a[1,2]=12
        self.a[2,1]=21
        self.assertTrue(isequal_(self.a, sparsearray([[11,12],[21,22]])))

class Indices(unittest.TestCase):
    def setUp(self):
        self.a = sparsearray(np.arange(1,5).reshape(2,2,order="F"))
        self.c = sparsearray(arange_(1,20).reshape(4,5,order="F"))

    def test01(self):
        with self.assertRaises(ValueError):
            self.a[100]=100
        self.a[3,3]=33
        self.assertTrue(isequal_(self.a.todense(), np.asarray([[1,3,0],[2,4,0],[0,0,33]])))

    def test02(self):
        self.assertTrue(isequal_(self.a[:], [1,2,3,4]))
        self.assertTrue(isequal_(self.a[1:4], [1,2,3,4]))

    @unittest.skip("")
    def test03(self):
        self.assertTrue(isequal_(self.a[:,:], [[1,3],[2,4]]))
        self.assertTrue(isequal_(self.a[1:2,1:2], [1,2,3,4]))

    def test04(self):
        a = sparsearray([1,2,3])
        a[5]=100
        a[3,3]=33
        self.assertEqual(a.shape, (3,5))
        self.assertTrue(isequal_(a.todense(), [[1,2,3,0,100],[0,0,0,0,0],[0,0,33,0,0]]))


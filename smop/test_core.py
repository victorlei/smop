# SMOP compiler runtime support library
# Copyright 2014 Victor Leikehman

# MIT license
import numpy as np
import unittest
from core import *

class Getitem(unittest.TestCase):
    def setUp(self):
        self.a = matlabarray(arange(1,20).reshape(4,5,order="F"))
    # 2-dim
    def test01(self):
        self.assertEqual(self.a[1,1],1)

    def test02(self):
        self.assertEqual(self.a[4,5],20)

    def test03(self):
        self.assertTrue(isequal(self.a[:,:],self.a))

    # 1-dim
    def test04(self):
        a = matlabarray(arange(1,20).reshape(4,5,order="F"))
        aa = a[:]
        bb = matlabarray(arange(1,20).reshape(-1,1,order="F"))
        self.assertTrue(isequal(aa,bb))

    def test05(self):
        #import pdb; pdb.set_trace()
        z = [[11,22,33,44],
             [11,22,33,44],
             [11,22,33,44],
             [11,22,33,44]]
        a = matlabarray([11,22,33,44], dtype=int)
        self.assertTrue(isequal(a[  [1,1,1,1] , 1:4], matlabarray(z)))
        self.assertTrue(isequal(a[  [1,1,1,1],   : ], matlabarray(z)))
        #self.assertTrue(isequal(a[ [[1,1,1,1]], 1:4], matlabarray([z])))
            
    def test06(self):
        a=copy(0)
        a[6]=666
        self.assertTrue(isequal(a, [[0.,0.,0.,0.,0.,666.]]))

class Expand(unittest.TestCase):
    """
    Expand on index error  
    """
    def setUp(self):
        self.a = matlabarray(zeros(1,4))

    def test01(self):
        self.a[1,5]=1
        self.assertTrue(isequal(self.a,
                                 matlabarray([[0,0,0,0,1]])))

    def test02(self):
        #with self.assertRaises(IndexError):
        a = matlabarray(zeros(1,4))
        a[5]=1 # single index
        self.assertTrue(isequal(a,
                                 matlabarray([[0,0,0,0,1]])))

    #@unittest.skip("")
    def test03(self):
        a=zeros(1,4) 
        a[1:10:4]=1
        "[[ 1.  0.  0.  0.  1.  0.  0.  0.  1.  0.]]"

    #@unittest.skip("")
    def test04(self):
        a=zeros(1,4) 
        with self.assertRaises(IndexError):
            a[5,5]=1
            b = matlabarray(
                [[0,0,0,0,0],
                 [0,0,0,0,0],
                 [0,0,0,0,0],
                 [0,0,0,0,0],
                 [0,0,0,0,1]])
            self.assertTrue(isequal(a,b))
 
class Strread(unittest.TestCase):
    def test01(self):
        a = strread("0.11 0.22 0.33")
        self.assertTrue(isequal(a,[[0.11,0.22,0.33]]))

    def test02(self):
        a,b,c = strread("0.11 0.22 0.33",nargout=3)
        self.assertEqual(a,0.11)
        self.assertEqual(b,0.22)
        self.assertEqual(c,0.33)

class Core(unittest.TestCase):
    def setUp(self):
        self.a = arange(1,10).reshape(2,5,order="F")

    def test01(self):
        b = abs(-self.a)
        self.assertTrue(isequal(self.a,b))

    def test02(self):
        b = ceil(self.a)
        self.assertTrue(isequal(self.a,b))

    def test03(self):
        b = false(2,3)
        self.assertTrue(isequal(size(b), [[2,3]]))

    def test_zeros(self):
        self.assertEqual(zeros(), 0.0)
        self.assertTrue(isequal(zeros(2), zeros(2,2)))
        self.assertTrue(isequal(zeros(2,2), zeros(2,2)))

        
#class Copy(unittest.TestCase):
#    def setUp(self):
#        self.a = zeros(1,4)
#
#    def test01(self):
#        b=self.a.copy()
#        print self.a
#        print b
#        self.assertTrue(isequal(self.a,b))
#        b[1,1]=123
#        self.assertTrue(not isequal(self.a,b))
#        #c=b.copy()
#        #c[1,:]=1
#        #self.assertTrue(not isequal(b,c))

if __name__ == "__main__":
    unittest.main()

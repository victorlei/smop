# SMOP compiler runtime support library
# Copyright 2014 Victor Leikehman

# MIT license
import numpy as np
import unittest
from runtime import *

class Getitem(unittest.TestCase):
    def setUp(self):
        self.a = matlabarray(arange_(1,20).reshape(4,5,order="F"))
    # 2-dim
    def test01(self):
        self.assertEqual(self.a[1,1],1)

    def test02(self):
        self.assertEqual(self.a[4,5],20)

    def test03(self):
        self.assertTrue(isequal_(self.a[:,:],self.a))

    # 1-dim
    def test04(self):
        a = matlabarray(arange_(1,20).reshape(4,5,order="F"))
        aa = a[:]
        bb = matlabarray(arange_(1,20).reshape(-1,1,order="F"))
        self.assertTrue(isequal_(aa,bb))

    def test05(self):
        #import pdb; pdb.set_trace()
        z = [[11,22,33,44],
             [11,22,33,44],
             [11,22,33,44],
             [11,22,33,44]]
        a = matlabarray([11,22,33,44], dtype=int)
        self.assertTrue(isequal_(a[  [1,1,1,1] , 1:4], matlabarray(z)))
        self.assertTrue(isequal_(a[  [1,1,1,1],   : ], matlabarray(z)))
        #self.assertTrue(isequal_(a[ [[1,1,1,1]], 1:4], matlabarray([z])))
            
    def test06(self):
        a=copy_(0)
        a[6]=666
        self.assertTrue(isequal_(a, [[0.,0.,0.,0.,0.,666.]]))

class Expand(unittest.TestCase):
    """
    Expand on index error  
    """
    def setUp(self):
        self.a = matlabarray(zeros_(1,4))

    def test01(self):
        self.a[1,5]=1
        self.assertTrue(isequal_(self.a,
                                 matlabarray([[0,0,0,0,1]])))

    def test02(self):
        #with self.assertRaises(IndexError):
        a = matlabarray(zeros_(1,4))
        a[5]=1 # single index
        self.assertTrue(isequal_(a,
                                 matlabarray([[0,0,0,0,1]])))

    #@unittest.skip("")
    def test03(self):
        a=zeros_(1,4) 
        a[1:10:4]=1
        "[[ 1.  0.  0.  0.  1.  0.  0.  0.  1.  0.]]"

    #@unittest.skip("")
    def test04(self):
        a=zeros_(1,4) 
        with self.assertRaises(IndexError):
            a[5,5]=1
            b = matlabarray(
                [[0,0,0,0,0],
                 [0,0,0,0,0],
                 [0,0,0,0,0],
                 [0,0,0,0,0],
                 [0,0,0,0,1]])
            self.assertTrue(isequal_(a,b))
 
class Strread(unittest.TestCase):
    def test01(self):
        a = strread_("0.11 0.22 0.33")
        self.assertTrue(isequal_(a,[[0.11,0.22,0.33]]))

    def test02(self):
        a,b,c = strread_("0.11 0.22 0.33",nargout=3)
        self.assertEqual(a,0.11)
        self.assertEqual(b,0.22)
        self.assertEqual(c,0.33)

class Core(unittest.TestCase):
    def setUp(self):
        self.a = arange_(1,10).reshape(2,5,order="F")

    def test01(self):
        b = abs_(-self.a)
        self.assertTrue(isequal_(self.a,b))

    def test02(self):
        b = ceil_(self.a)
        self.assertTrue(isequal_(self.a,b))

    def test03(self):
        b = false_(2,3)
        self.assertTrue(isequal_(size_(b), [[2,3]]))

    def test_zeros(self):
        self.assertEqual(zeros_(), 0.0)
        self.assertTrue(isequal_(zeros_(2), zeros_(2,2)))
        self.assertTrue(isequal_(zeros_(2,2), zeros_(2,2)))

        
#class Copy(unittest.TestCase):
#    def setUp(self):
#        self.a = zeros_(1,4)
#
#    def test01(self):
#        b=self.a.copy()
#        print self.a
#        print b
#        self.assertTrue(isequal_(self.a,b))
#        b[1,1]=123
#        self.assertTrue(not isequal_(self.a,b))
#        #c=b.copy()
#        #c[1,:]=1
#        #self.assertTrue(not isequal_(b,c))

if __name__ == "__main__":
    unittest.main()

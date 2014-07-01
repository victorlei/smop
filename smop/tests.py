# SMOP compiler runtime support library
# Copyright 2014 Victor Leikehman

# MIT license
import numpy as np
import unittest
from runtime import *

"""
Matlab matrices differ from numpy arrays:

o Array indices start with one, not with zero.  Accordingly, the last
  element of an N-element array is N, not N-1 as in C.

o Matrix elements are ordered column-first, aka "Fortran" order.

o Arrays auto-expand on out-of-bound lhs indexing.

o Array data is not shared by copying or slice indexing. Instead there
  is copy-on-write.

o There are no zero or one-dimensional arrays. Scalars are
  two-dimensional rather than zero=dimensional as in numpy.

o Strings are not yet supported.
"""

class Index(unittest.TestCase):
    def setUp(self):
        self.a = matlabarray(arange_(1,4).reshape(2,2,order="F"))

    def test01(self):
        """
        One-dimensional LHS indexing of multi-dimensional array,
        without expansion.  Using scalar index.
        """
        self.a[4]=9
        self.assertTrue(isequal_(self.a, [[1, 3],[2, 9]]))

    def test02(self):
        """
        Multi-dimensional LHS indexing, without expansion.
        """
        a=matlabarray(zeros_(2,2))
        a[1,1]=11
        a[2,2]=22
        a[1,2]=12
        a[2,1]=21
        self.assertTrue(isequal_(a, [[11,12],[21,22]]))

class Indices(unittest.TestCase):
    """Test matlabarray.compute_indices"""
    def setUp(self):
        self.a = matlabarray(arange_(1,20).reshape(4,5,order="F"))

    # one dimension
    def test01(self):
        indices = self.a.compute_indices(100)
        self.assertEqual(indices,(99,))

    def test02(self):
        indices = self.a.compute_indices(slice(None,None,None))
        self.assertEqual(indices,(slice(0,20,1),))
        
    def test03(self):
        indices = self.a.compute_indices(slice(0,20,1))
        self.assertEqual(indices,(slice(0,20,1),))

    # two dimensions
    def test04(self):
        indices = self.a.compute_indices((slice(None,None,None),
                                          slice(None,None,None)))
        self.assertEqual(indices,(slice(0,4,1),slice(0,5,1)))

    # broadcast
    #def test05(self):
    #    indices = self.a.compute_indices(([1,1,1],
    #                                      slice(None,None,None)))
    #    
    #    (array([0, 0, 0]), slice(0, 5, 1))
        

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
        self.assertTrue(isequal_(self.a[:],arange_(1,20)))

    def test05(self):
        z = [[1,2,3,4],
             [1,2,3,4],
             [1,2,3,4]]
        a = matlabarray([[1,2,3,4]])
        self.assertTrue(isequal_(a[[1,1,1],1:4], z))
        self.assertTrue(isequal_(a[[[1,1,1]],1:4], z))
        self.assertTrue(isequal_(a[[1,1,1], : ], z))
            
class Slice(unittest.TestCase):
    def setUp(self):
        self.a = matlabarray(arange_(1,4).reshape(2,2,order="F"))

    def test01(self):
        self.assertTrue(isequal_(self.a[1:4],[1,2,3,4]))
        self.assertEqual(self.a[1,1], 1)
        self.assertEqual(self.a[2,1], 2)
        self.assertEqual(self.a[1,2], 3)
        self.assertEqual(self.a[2,2], 4)

    def test02(self):
        self.a[1,1] = 11
        self.a[2,1] = 22
        self.a[1,2] = 33
        self.a[2,2] = 44
        self.assertTrue(isequal_(self.a[1:4],[11,22,33,44]))

    def test03(self):
        self.a[1] = 11
        self.a[2] = 22
        self.a[3] = 33
        self.a[4] = 44
        self.assertTrue(isequal_(self.a[1:4],[11,22,33,44]))

    def test04(self):
        #with self.assertRaises(ValueError):
        self.a[1:4]=[11,22,33,44]
        self.assertTrue(isequal_(self.a[1:4],[11,22,33,44]))

    def test05(self):
        self.a[:,:]=[[11,33],[22,44]]
        self.assertTrue(isequal_(self.a[1:4],[11,22,33,44]))

    def test06(self):
        self.a[:]=[11,22,33,44]
        self.assertTrue(isequal_(self.a[1:4],[11,22,33,44]))

    def test07(self):
        self.a[::3]=[11,44]
        self.assertTrue(isequal_(self.a[1:4],[11,2,3,44]))

    def test08(self):
        self.a[1:4:3]=[11,44]
        self.assertTrue(isequal_(self.a[1:4],[11,2,3,44]))

    @unittest.skip("")
    def test_007(self):
        """
        One-dimensional LHS indexing without expansion, 
        using list index.
        """
        a=zeros_(2,2)
        a[[1,4]]=1
        self.assertEqual(str(a), "[[ 1.  0.]\n [ 0.  1.]]")

    @unittest.skip("")
    def test_008(self):
        a=zeros_(2,2)
        a[[4,3,2,1]]=[1,2,3,4]
        self.assertEqual(str(a), "[[ 1.  2.]\n [ 3.  4.]]")



    @unittest.skip("")
    def test_010(self):
        a=zeros_(2,2)
        a[2,:]=[1,2]
        self.assertEqual(str(a), "[[ 0.  0.]\n [ 1.  2.]]")

    @unittest.skip("")
    def test_011(self):
        a=zeros_(2,2)
        a[2,1:2]=[1,2]
        self.assertEqual(str(a), "[[ 0.  0.]\n [ 1.  2.]]")

    @unittest.skip("")
    def test_012(self):
        a=zeros_(2,2)
        a[2,[1,2]]=[1,2]
        self.assertEqual(str(a), "[[ 0.  0.]\n [ 1.  2.]]")

    @unittest.skip("")
    def test_013(self):
        a=zeros_(2,2)
        a[:,:]=[[1,2],[3,4]]
        self.assertEqual(str(a), "[[ 1.  2.]\n [ 3.  4.]]")

    @unittest.skip("")
    def test_014(self):
        a=zeros_(2,2)  
        with self.assertRaises(ValueError):
            a[:,:]=[1,2,3,4]

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
        with self.assertRaises(NotImplementedError):
            self.a[5]=1 # single index

    @unittest.skip("")
    def test03(self):
        a=zeros_(1,4) 
        a[1:10:4]=1
        "[[ 1.  0.  0.  0.  1.  0.  0.  0.  1.  0.]]"

    def test04(self):
        self.a[5,5]=1
        b = matlabarray(
            [[0,0,0,0,0],
             [0,0,0,0,0],
             [0,0,0,0,0],
             [0,0,0,0,0],
             [0,0,0,0,1]])
        self.assertTrue(isequal_(self.a,b))

class Strread(unittest.TestCase):
    def test01(self):
        a = strread("0.11 0.22 0.33")
        self.assertTrue(isequal_(a,[[0.11,0.22,0.33]]))

    def test02(self):
        a,b,c = strread("0.11 0.22 0.33",nargout=3)
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
        self.assertEqual(size_(b), (2,3))

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

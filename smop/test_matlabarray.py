import unittest
from core import *

class test_matlabarray(unittest.TestCase):
    """Expanding matlabarray"""
    def test010(self):
        """Two-dimensional assignment to []"""
        a = matlabarray()
        a[a.shape[0]+1,[1,2,3]] = [123,456,789]
        a[a.shape[0]+1,[1,2,3]] = [123,456,789]
        a[a.shape[0]+1,[1,2,3]] = [123,456,789]
        self.assertTrue(isequal (a, [[123,456,789],
                                     [123,456,789],
                                     [123,456,789]]))
    def test020(self):
        """Two-dimensional assignment to []
           Expand, use list [1,2,3] for indexing"""
        a = matlabarray()
        a[a.shape[0]+1,[1,2,3]] = 123
        a[a.shape[0]+1,[1,2,3]] = 123
        a[a.shape[0]+1,[1,2,3]] = 123
        self.assertTrue(isequal (a, [[123,123,123],
                                     [123,123,123],
                                     [123,123,123]]))

    def test030(self):
        """Two-dimensional assignment to []
           Expand, use slice 1:3 for indexing"""
        a = matlabarray()
        #import pdb; pdb.set_trace()
        a[a.shape[0]+1,1:3] = 123
        a[a.shape[0]+1,1:3] = 123
        a[a.shape[0]+1,1:3] = 123
        #print a.shape
        self.assertTrue(isequal (a, [[123,123,123],
                                     [123,123,123],
                                     [123,123,123]]))
    #@unittest.skip("FIXME")
    def test040(self):
        a = matlabarray()
        with self.assertRaises(IndexError):
            a[a.shape[0]+1,:] = 123
            a[a.shape[0]+1,:] = 123
            a[a.shape[0]+1,:] = 123
            self.assertTrue(isequal (a, [[123],
                                         [123],
                                         [123]]))
    @unittest.skip("wonders of matlab")
    def test050(self):
        """
        Compare to test060

        octave> a=[]
        a = []
        octave> a(:,:)=99
        a =  99
        octave> a
        a =  99
        octave> size(a)
        ans =

        1   1
        """
        a = matlabarray()
        a[:,:] = 99
        self.assertTrue(isequal (a.item(0), 99))

    def test060(self):
        """One-dimensional assignment to empty array

        octave> a=[]
        a = []
        octave> a(:)=99
        a = []
        octave> a
        a = []
        """
        a = matlabarray()
        with self.assertRaises(IndexError):
            a[:] = 99
            self.assertTrue(isempty (a))

    #@unittest.skip("wonders of matlab")
    def test062(self):
        """One-dimensional assignment to empty array

        octave> a=[]
        a = []
        octave> a(:)=[1 2 3]
        error: A(I) = X: X must have the same size as I
        """
        a = matlabarray()
        with self.assertRaises(Exception):
            a[:] = [1,2,3]

    #@unittest.skip("wonders of matlab")
    def test064(self):
        """One-dimensional assignment to empty array

        octave> a=[]
        a = []
        octave> a(1:3)=[1 2 3]
        1 2 3
        """
        a = matlabarray()
        a[1:3] = [1,2,3]
        self.assertTrue(isequal (a, [1,2,3]))

    def test070(self):
        """
        wonders of matlab

        octave> c=[]
        c = []
        octave> c(1:end)=9
        c = []
        """
        a = matlabarray()
        a[1:a.shape[0]] = 9
        self.assertTrue(isempty (a))

    @unittest.skip("wonders of matlab")
    def test080(self):
        """
        octave> a=[]
        a = []
        octave> a(1:end,5) = 5
        a = [](0x5)        % notice 0x5
        """

    def test084(self):
        """
        octave> a=[]
        a = []
        octave> a(:,5) = 5
        a =

        0   0   0   0   5
        """

    def test090(self):
        a = matlabarray([[11,22,33]])
        a[4] = 44
        self.assertTrue(isequal (a,[[11,22,33,44]]))

    def test092(self):
        a = matlabarray([[11,22,33,44]])
        a[5:7] = [55,66,77]
        self.assertTrue(isequal (a,[[11,22,33,44,55,66,77]]))

    def test094(self):
        a = matlabarray([[11,22,33,44,55,66,77]])
        a[[8,9]] = [88,99]
        self.assertTrue(isequal (a,[[11,22,33,44,55,66,77,88,99]]))

    def test100(self):
        a = matlabarray([[1,3],
                         [2,4]])
        #a[: , a.shape[1]+1] = [5,6]
        a[: , 3] = [5,6]
        self.assertTrue(isequal (a,[[1,3,5],
                                    [2,4,6]]))

    def test110(self):
        a = zeros (4,4,dtype=int)
        a[2:3,2:3] = 1
        #print a
        self.assertTrue(isequal (a,[[0,0,0,0],
                                    [0,1,1,0],
                                    [0,1,1,0],
                                    [0,0,0,0]]))
if __name__ == "__main__":
    unittest.main()
# vim:et:sw=4:si:

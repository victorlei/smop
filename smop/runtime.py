# SMOP compiler runtime support library
# Copyright 2014 Victor Leikehman

# MIT license
from numpy import inf
import numpy as np
import os,sys,copy
from scipy.io import loadmat
import unittest

class matlabarray(np.ndarray):
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

    o Single subscript implies ravel.

    o Broadcasting rules are different

    o Strings are not yet supported.
    """
    def __new__(cls,input_array=[],**kwargs):
        obj = np.asarray(input_array,order="F").view(cls).copy(order="F")
        if obj.size == 0:
            obj.shape = (0,0)
        return obj

    #def __array_finalize__(self,obj):
    #    self.delegate = obj

    def __copy__(self):
        return np.ndarray.copy(self,order="F")

    def compute_indices(self,index):
        if not isinstance(index,tuple):
           index = index,
        if len(index) != 1 and len(index) != self.ndim:
            raise IndexError
        indices = []
        for i,ix in enumerate(index):
            if ix == slice(None,None):
                indices.append(ix)
            elif ix.__class__ is slice:
                if len(index) == 1:
                    n = self.size
                else:
                    n = self.shape[i]
                #start,stop,step = ix.indices(n)
                indices.append(slice((ix.start or 1)-1,
                                     ix.stop   or n,
                                     ix.step   or 1))
            else:
                try:
                    indices.append(int(ix)-1)
                except:
                    indices.append(np.asarray(ix)-1)
        try:
            indices[0] = indices[0].reshape(-1,1)
        except:
            pass
        return tuple(indices)

    def __getslice__(self,i,j):
        if i == 0 and j == sys.maxsize:
            return self.reshape(-1,1,order="F")
        return self.__getitem__(slice(i,j))

    def __getitem__(self,index):
        #import pdb; pdb.set_trace()
        indices = self.compute_indices(index)
        if len(indices) == 1:
            return np.ndarray.__getitem__(self.reshape(-1,order="F"),indices)
        else:
            return np.ndarray.__getitem__(self,indices)

    def __setslice__(self,i,j,value):
        if i == 0 and j == sys.maxsize:
            index = slice(None,None)
        else:
            index = slice(i,j)
        self.__setitem__(index,value)
        
    def sizeof(self,ix):
        if isinstance(ix,int):
            n = ix+1
        elif isinstance(ix,slice):
            n = ix.stop
        elif isinstance(ix,(list,np.ndarray)):
            n = max(ix)+1
        else:
            assert 0,ix
        return n

    def __setitem__(self,index,value):
        #import pdb; pdb.set_trace()
        indices = self.compute_indices(index)
        try:
            if len(indices) == 1:
                foo= np.asarray(self).reshape(-1,order="F")
                foo.__setitem__(indices,value)
            else:
                np.asarray(self).__setitem__(indices,value)
        except (ValueError,IndexError):
            #import pdb; pdb.set_trace()
            if len(indices) == 1:
                if self.shape.count(1) < self.ndim-1:
                    raise IndexError
                # self.shape has at most one non-degenerated dimension
                n = self.sizeof(indices[0]) # zero-based
                new_shape = [(1 if s==1 else n) for s in self.shape]
                self.resize(new_shape,refcheck=0)
                np.asarray(self).reshape(-1,order="F").__setitem__(indices,value)
            else:
                if 0 in self.shape:
                    new_shape = [self.sizeof(s) for s in indices]
                else:
                    new_shape = list(self.shape)
                    if self.flags["C_CONTIGUOUS"]:
                        new_shape[0] = self.sizeof(indices[0])
                    elif self.flags["F_CONTIGUOUS"]:
                        new_shape[-1] = self.sizeof(indices[-1])
                self.resize(new_shape,refcheck=0)
                np.asarray(self).__setitem__(indices,value)

    def __repr__(self):
        return repr(np.asarray(self))

    def __str__(self):
        return str(np.asarray(self))
 
    def __add__(self,other):
        return np.ndarray.__add__(self,other)

    def __neg__(self):
        return np.ndarray.__neg__(self)

class test_matlabarray(unittest.TestCase):
    """Expanding matlabarray"""
    def test01(self):
        a = matlabarray()
        a[a.shape[0]+1,[1,2,3]] = [123,456,789]
        a[a.shape[0]+1,[1,2,3]] = [123,456,789]
        a[a.shape[0]+1,[1,2,3]] = [123,456,789]
        self.assertTrue(isequal_(a, [[123,456,789],
                                     [123,456,789],
                                     [123,456,789]]))
    def test02(self):
        """Expand, use list [1,2,3] for indexing"""
        a = matlabarray()
        a[a.shape[0]+1,[1,2,3]] = 123
        a[a.shape[0]+1,[1,2,3]] = 123
        a[a.shape[0]+1,[1,2,3]] = 123
        self.assertTrue(isequal_(a, [[123,123,123],
                                     [123,123,123],
                                     [123,123,123]]))

    def test03(self):
        """Expand, use slice 1:3 for indexing"""
        a = matlabarray()
        #import pdb; pdb.set_trace()
        a[a.shape[0]+1,1:3] = 123
        a[a.shape[0]+1,1:3] = 123
        a[a.shape[0]+1,1:3] = 123
        #print a.shape
        self.assertTrue(isequal_(a, [[123,123,123],
                                     [123,123,123],
                                     [123,123,123]]))
    @unittest.skip("")
    def test04(self):
        a = matlabarray()
        a[a.shape[0]+1,:] = 123
        a[a.shape[0]+1,:] = 123
        a[a.shape[0]+1,:] = 123
        self.assertTrue(isequal_(a, [[123],
                                     [123],
                                     [123]]))
    @unittest.skip("wonders of matlab")
    def test05(self):
        """
        octave:48> a=[]
        a = [](0x0)
        octave:49> a(:,:)=99
        a =  99
        octave:50> a
        a =  99
        octave:51> size(a)
        ans =

        1   1
        """
        a = matlabarray()
        a[:,:] = 99
        self.assertTrue(isequal_(a.item(0), 99))

    @unittest.skip("wonders of matlab")
    def test06(self):
        """
        octave:52> a=[]
        a = [](0x0)
        octave:53> a(:)=99
        a = [](0x0)
        octave:54> a
        a = [](0x0)

        """

    @unittest.skip("wonders of matlab")
    def test07(self):
        """
        octave:38> c=[]
        c = [](0x0)
        octave:39> c(:)=[2 3 5]
        error: A(I) = X: X must have the same size as I
        """
        a = matlabarray()
        a[:] = arange_(1,3).T
        self.assertTrue(isequal_(a,arange_(1,3)))

    @unittest.skip("wonders of matlab")
    def test08(self):
        """
        octave:44> a=[]
        a = [](0x0)
        octave:45> a(1:end,5) = 5
        a = [](0x5)        % notice 0x5
        octave:46> a=[]
        a = [](0x0)
        octave:47> a(:,5) = 5
        a =

        0   0   0   0   5
        """

    def test09(self):
        a = matlabarray([[11,22,33]])
        a[4] = 44
        self.assertTrue(isequal_(a,[[11,22,33,44]]))

    def test09a(self):
        a = matlabarray([[11,22,33,44]])
        a[5:7] = [55,66,77]
        self.assertTrue(isequal_(a,[[11,22,33,44,55,66,77]]))

    def test09b9(self):
        a = matlabarray([[11,22,33,44,55,66,77]])
        a[[8,9]] = [88,99]
        self.assertTrue(isequal_(a,[[11,22,33,44,55,66,77,88,99]]))

    def test10(self):
        a = matlabarray([[1,3],
                         [2,4]])
        #a[: , a.shape[1]+1] = [5,6]
        a[: , 3] = [5,6]
        self.assertTrue(isequal_(a,[[1,3,5],
                                    [2,4,6]]))

    def test11(self):
        a = zeros_(4,4,dtype=int)
        a[2:3,2:3] = 1
        #print a
        self.assertTrue(isequal_(a,[[0,0,0,0],
                                    [0,1,1,0],
                                    [0,1,1,0],
                                    [0,0,0,0]]))

def abs_(a):
    return np.abs(np.asanyarray(a))

def arange_(start,stop,step=1,**kwargs):
    return np.arange(start,stop+1,step,**kwargs).reshape(1,-1)

def ceil_(a):
    return np.ceil(np.asanyarray(a))

def copy_(a):
    return np.asanyarray(a).copy(order="F")

def disp_(*args):
    print (args)

false = False

def false_(*args):
    if len(args) == 1:
        args += args
    return np.zeros(args,dtype=bool,order="F")

def find_(a,nargout=2):
    i,j = np.asanyarray(a).nonzero()
    return (i+1),(j+1)

def floor_(a):
    return np.floor_(np.asanyarray(a))

def fullfile_(*args):
    return os.path.join(*args)

def intersect_(a,b,nargout=1):
    if nargout == 1:
        c = sorted(set(a) & set(b))
        if isinstance(a,str):
            return "".join(c)
        elif isinstance(a,list):
            return c
        else:
            # FIXME: the result is a column vector if
            # both args are column vectors; otherwise row vector
            return np.array(c)
    raise ErrorNotImplemented    

#def inf_(*args):
#    t = np.empty(np.prod(args))
#    t[:] = np.inf
#    return t.reshape(args)
#
#Inf_ = inf_
#
#def int_(t):
#    return np.int(t)
#
#def int32_(t):
#    return np.int(t)
#
def isempty_(a):
    try:
        return 0 in np.asarray(a).shape
    except AttributeError:
        return False

def isequal_(a,b):
    return np.array_equal(np.asanyarray(a),
                          np.asanyarray(b))
                          

def length_(a):
    try:
        return max(np.asarray(a).shape)
    except ValueError:
        return 1

def load_(a):
    return loadmat(a) # FIXME

def max_(a, d=0, nargout=0):
    if d or nargout:
        raise ErrorNotImplemented()
    return np.amax(a)

def min_(a, d=0, nargout=0):
    if d or nargout:
        raise ErrorNotImplemented()
    return np.amin(a)

def numel_(a):
    return np.asarray(a).size

def ones_(*args,**kwargs):
        return matlabarray(np.ones(args,dtype="int",**kwargs))

def rand_(*args,**kwargs):
    if not args:
        return np.random.rand()
    if len(args) == 1:
        args += args
    return np.random.rand(np.prod(args)).reshape(args,order="F")

def ravel_(a):
    return np.asanyarray(a).reshape(-1,1)

def round_(a):
    return np.round(np.asanyarray(a))

def size_(a, b=0, nargout=2):
    s = np.asarray(a).shape
    if s is ():
        return 1 if b else (1,)*nargout
    # a is not a scalar
    try:
        return s[b-1] if b else s
    except IndexError:
        return 1

def strread_(s, format="", nargout=1):
    if format == "":
        a = [float(x) for x in s.split()]
        return tuple(a) if nargout > 1 else np.asanyarray([a])
    raise ErrorNotImplemented

def strrep_(a,b,c):
    if isinstance(a,str):
        return a.replace(b,c)
    raise ErrorNotImplemented # cell arrays

def sum_(a):
    return np.asanyarray(a).sum()

def true_(*args):
    if len(args) == 1:
        args += args
    return matlabarray(np.ones(args,dtype=bool,order="F"))

def zeros_(*args,**kwargs):
    if not args:
        return 0.0
    if len(args) == 1:
        args += args
    return matlabarray(np.zeros(args,order="F",**kwargs))

if __name__ == "__main__":
    """
    a = matlabarray()
    import pdb; pdb.set_trace()
    a[a.shape[0]+1,1:3] = 123
    a[a.shape[0]+1,1:3] = 123
    print(a)
    """
    unittest.main()

# vim:et:sw=4:si:

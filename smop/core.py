# SMOP compiler runtime support library
# Copyright 2014 Victor Leikehman

# MIT license

import __builtin__

import numpy
from numpy import sqrt
from numpy.fft import fft2
from numpy.linalg import inv
from numpy.linalg import qr  as _qr 
try:
    from scipy.linalg import schur as _schur
except ImportError:
    pass
import numpy as np

import os,sys,copy,time
from sys import stdin,stdout,stderr
try:
    from scipy.io import loadmat
except:
    pass
import unittest

def isvector_or_scalar(a):
    """
    one-dimensional arrays having shape [N],
    row and column matrices having shape [1 N] and
    [N 1] correspondingly, and their generalizations
    having shape [1 1 ... N ... 1 1 1].
    Scalars have shape [1 1 ... 1].
    Empty arrays dont count
    """
    try:
        return a.size and a.ndim-a.shape.count(1) <= 1
    except:
        return False
def isvector(a):
    """
    one-dimensional arrays having shape [N],
    row and column matrices having shape [1 N] and
    [N 1] correspondingly, and their generalizations
    having shape [1 1 ... N ... 1 1 1]
    """
    try:
        return a.ndim-a.shape.count(1) == 1
    except:
        return False

class matlabarray(np.ndarray):
    """
    >>> matlabarray()
    matlabarray([], shape=(0, 0), dtype=float64)
    """

    def __new__(cls,a=[],dtype=None):
        obj = np.array(a,
                       dtype=dtype,
                       copy=False,
                       order="F",
                       ndmin=2).view(cls).copy(order="F")
        if obj.size == 0:
            obj.shape = (0,0)
        return obj

    #def __array_finalize__(self,obj):

    def __copy__(self):
        return np.ndarray.copy(self,order="F")

    def __iter__(self):
        """ must define iter or char won't work"""
        return np.asarray(self).__iter__()

    def compute_indices(self,index):
        if not isinstance(index,tuple):
           index = index,
        if len(index) != 1 and len(index) != self.ndim:
            raise IndexError
        indices = []
        for i,ix in enumerate(index):
            if ix.__class__ is end:
                indices.append(self.shape[i]-1+ix.n)
            elif ix.__class__ is slice:
                if self.size == 0 and ix.stop is None:
                    raise IndexError
                if len(index) == 1:
                    n = self.size
                else:
                    n = self.shape[i]
                indices.append(np.arange((ix.start or 1)-1,
                                          ix.stop  or n,
                                          ix.step  or 1,
                                          dtype=int))
            else:
                try:
                    indices.append(int(ix)-1)
                except:
                    indices.append(np.asarray(ix).astype("int32")-1)
        if len(indices) == 2 and isvector(indices[0]) and isvector(indices[1]):
            indices[0].shape = (-1,1)
            indices[1].shape = (-1,)
        return tuple(indices)

    def __getslice__(self,i,j):
        if i == 0 and j == sys.maxsize:
            return self.reshape(-1,1,order="F")
        return self.__getitem__(slice(i,j))

    def __getitem__(self,index):
        return matlabarray(self.get(index))

    def get(self,index):
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
        if not isinstance(n,int):
            raise IndexError
        return n 

    def __setitem__(self,index,value):
        #import pdb; pdb.set_trace()
        indices = self.compute_indices(index)
        try:
            if len(indices) == 1:
                np.asarray(self).reshape(-1,order="F").__setitem__(indices,value)
            else:
                np.asarray(self).__setitem__(indices,value)
        except (ValueError,IndexError):
            #import pdb; pdb.set_trace()
            if not self.size:
                new_shape = [self.sizeof(s) for s in indices]
                self.resize(new_shape,refcheck=0)
                np.asarray(self).__setitem__(indices,value)
            elif len(indices) == 1:
                # One-dimensional resize is only implemented for
                # two cases:
                #
                # a. empty matrices having shape [0 0]. These
                #    matries may be resized to any shape.  A[B]=C
                #    where A=[], and B is specific -- A[1:10]=C
                #    rather than A[:]=C or A[1:end]=C
                if self.size and not isvector_or_scalar(self):
                    raise IndexError("One-dimensional resize "
                                     "works only on vectors, and "
                                     "row and column matrices")
                # One dimensional resize of scalars creates row matrices
                # ai = 3
                # a(4) = 1
                # 3 0 0 1
                n = self.sizeof(indices[0]) # zero-based
                if max(self.shape) == 1:
                    new_shape = list(self.shape)
                    new_shape[-1] = n
                else:
                    new_shape = [(1 if s==1 else n) for s in self.shape]
                self.resize(new_shape,refcheck=0)
                np.asarray(self).reshape(-1,order="F").__setitem__(indices,value)
            else:
                new_shape = list(self.shape)
                if self.flags["C_CONTIGUOUS"]:
                    new_shape[0] = self.sizeof(indices[0])
                elif self.flags["F_CONTIGUOUS"]:
                    new_shape[-1] = self.sizeof(indices[-1])
                self.resize(new_shape,refcheck=0)
                np.asarray(self).__setitem__(indices,value)

    def __repr__(self):
        return self.__class__.__name__ + repr(np.asarray(self))[5:]

    def __str__(self):
        return str(np.asarray(self))
 
    def __add__(self,other):
        return matlabarray(np.asarray(self)+np.asarray(other))

    def __neg__(self):
        return matlabarray(np.asarray(self).__neg__())

class end(object):
    def __add__(self,n):
        self.n = n
        return self
    def __sub__(self,n):
        self.n = -n
        return self
####
class cellarray(matlabarray):
    """
    Cell array corresponds to matlab ``{}``


    """

    def __new__(cls, a=[]):
        """
        Create a cell array and initialize it with a.
        Without arguments, create an empty cell array.

        Parameters:
        a : list, ndarray, matlabarray, etc.

        >>> a=cellarray([123,"hello"])
        >>> print a.shape
        (1, 2)

        >>> print a[1]
        123

        >>> print a[2]
        hello
        """
        obj = np.array(a,
                       dtype=object,
                       order="F",
                       ndmin=2).view(cls).copy(order="F")
        if obj.size == 0:
            obj.shape = (0,0)
        return obj

    def __getitem__(self,index): 
        return self.get(index)

#    def __str__(self):
#        if self.ndim == 0:
#            return ""
#        if self.ndim == 1:
#            return "".join(s for s in self)
#        if self.ndim == 2:
#            return "\n".join("".join(s) for s in self)
#        raise NotImplementedError


class cellstr(matlabarray):
    """
    >>> s=cellstr(char('helloworldkitty').reshape(3,5))
    >>> s
    cellstr([['hello', 'world', 'kitty']], dtype=object)
    >>> print s
    hello
    world
    kitty
    >>> s.shape
    (1, 3)
    """

    def __new__(cls, a):
        """
        Given a two-dimensional char object,
        create a cell array where each cell contains
        a line.
        """
        obj = np.array(["".join(s) for s in a], 
                       dtype=object,
                       copy=False,
                       order="C",
                       ndmin=2).view(cls).copy(order="F")
        if obj.size == 0:
            obj.shape = (0,0)
        return obj

    def __str__(self):
        return "\n".join("".join(s) for s in self.reshape(-1))

    def __getitem__(self,index): 
        return self.get(index)


class char(matlabarray):
    """
    class char is a rectangular string matrix, which
    inherits from matlabarray all its features except
    dtype.

    >>> s=char()
    >>> s.shape
    (0, 0)

    >>> s=char('helloworld').reshape(2,5)
    >>> print s
    hello
    world

    >>> s.shape
    (2, 5)
    """

    def __new__(cls, a=""):
        if not isinstance(a,str):
            raise NotImplementedError
        obj = np.array(list(a),
                       dtype='|S1',
                       copy=False,
                       order="F",
                       ndmin=2).view(cls).copy(order="F")
        if obj.size == 0:
            obj.shape = (0,0)
        return obj

    def __getitem__(self,index): 
        return self.get(index)

    def __str__(self):
        if self.ndim == 0:
            return ""
        if self.ndim == 1:
            return "".join(s for s in self)
        if self.ndim == 2:
            return "\n".join("".join(s) for s in self)
        raise NotImplementedError

class struct_(object):
    def __init__(self,*args):
        for i in range(0,len(args),2):
            setattr(self,str(args[i]),args[i+1])

NA = numpy.NaN

def abs(a):
    return numpy.abs(a)

def all(a):
    return numpy.all(a)

def arange(start,stop,step=1,**kwargs):
    """
    >>> a=arange(1,10) # 1:10
    >>> size(a)
    matlabarray([[ 1, 10]])
    """
    return matlabarray(np.arange(start,
                                 stop+1,
                                 step,
                                 **kwargs).reshape(1,-1),**kwargs)
def cat(*args):
    return np.concatenate([matlabarray(a) for a in args],axis=1)

def ceil(a):
    return numpy.ceil(a)

def cell(*args):
    if len(args) == 1:
        args += args
    return cellarray(np.zeros(args,dtype=object,order="F"))

def clc():
    pass

def copy(a):
    return matlabarray(np.asanyarray(a).copy(order="F"))

def deal(a,**kwargs):
    #import pdb; pdb.set_trace()
    return tuple([ai for ai in a.flat])

def disp(*args):
    print (args)

def eig(a):
    u,v = np.linalg.eig(a)
    return u.T

def exist(a,b):
    if str(b) == 'builtin':
        return str(a) in globals()
    if str(b) == 'file':
        return os.path.exists(str(a))
    raise NotImplementedError

def false(*args):
    if not args:
        return False # or matlabarray(False) ???
    if len(args) == 1: 
        args += args
    return np.zeros(args,dtype=bool,order="F")

def find(a,n=None,d=None,nargout=1):
    if d:
        raise NotImplementedError

    # there is no promise that nonzero or flatnonzero
    # use or will use indexing of the argument without
    # converting it to array first.  So we use asarray
    # instead of asanyarray
    if nargout == 1:
        i = np.flatnonzero(np.asarray(a)).reshape(1,-1)+1
        if n is not None:
            i = i.take(n)
        return matlabarray(i)
    if nargout == 2:
        i,j = np.nonzero(np.asarray(a))
        if n is not None:
            i = i.take(n)
            j = j.take(n)
        return (matlabarray((i+1).reshape(-1,1)),
                matlabarray((j+1).reshape(-1,1)))
    raise NotImplementedError

def floor(a):
    return numpy.floor(a)

def fopen(*args):
    try:
        fp = open(*args)
        assert fp != -1
        return fp
    except:
        return -1

def fflush(fp):
    fp.flush()

def fprintf(fp,fmt,*args):
    if not isinstance(fp,file):
        fp = stdout
    fp.write(str(fmt) % args)

def fullfile(*args):
    return os.path.join(*args)

# implemented in "scripts/set/intersect.m"
#def intersect(a,b,nargout=1):
#    if nargout == 1:
#        c = sorted(set(a) & set(b))
#        if isinstance(a,str):
#            return "".join(c)
#        elif isinstance(a,list):
#            return c
#        else:
#            # FIXME: the result is a column vector if
#            # both args are column vectors; otherwise row vector
#            return np.array(c)
#    raise NotImplementedError
#
def iscellstr(a):
    # TODO return isinstance(a,cellarray) and all(ischar(t) for t in a.flat)
    return isinstance(a,cellarray) and all(isinstance(t,str) for t in a.flat)

def ischar(a):
    try:
        return a.dtype == "|S1"
    except AttributeError:
        return False
# ----------------------------------------------------
def isempty(a):
    try:
        return 0 in np.asarray(a).shape
    except AttributeError:
        return False

def isequal(a,b):
    return np.array_equal(np.asanyarray(a),
                          np.asanyarray(b))
                          
def isfield(a,b):
    return str(b) in a.__dict__.keys()

def ismatrix(a):
    return True

def isnumeric(a):
    return np.asarray(a).dtype in (int,float)

def isscalar(a):
    """np.isscalar returns True if a.__class__ is a scalar
    type (i.e., int, and also immutable containers str and
    tuple, but not list.) Our requirements are different"""
    try:
        return a.size == 1
    except AttributeError:
        return np.isscalar(a)

def length(a):
    try:
        return __builtin__.max(np.asarray(a).shape)
    except ValueError:
        return 1

try:
    def load(a):
        return loadmat(a) # FIXME
except:
    pass

def max(a, d=0, nargout=0):
    if d or nargout:
        raise NotImplementedError
    return np.amax(a)

def min(a, d=0, nargout=0):
    if d or nargout:
        raise NotImplementedError
    return np.amin(a)

def mod(a,b):
    try:
        return a % b
    except ZeroDivisionError:
        return a

def ndims(a):
    return np.asarray(a).ndim

def numel(a):
    return np.asarray(a).size

def ones(*args,**kwargs):
    if not args:
        return 1.0
    if len(args) == 1:
        args += args
    return matlabarray(np.ones(args,order="F",**kwargs))

#def primes2(upto):
#    primes=np.arange(2,upto+1)
#    isprime=np.ones(upto-1,dtype=bool)
#    for factor in primes[:int(math.sqrt(upto))]:
#        if isprime[factor-2]: isprime[factor*2-2::factor]=0
#    return primes[isprime]
#
#def primes(*args):
#    return _primes.primes(*args)

def qr(a):
    return matlabarray(_qr(np.asarray(a)))

def rand(*args,**kwargs):
    if not args:
        return np.random.rand()
    if len(args) == 1:
        args += args
    try:
        return np.random.rand(np.prod(args)).reshape(args,order="F")
    except:
        pass

def rand(*args,**kwargs):
    if not args:
        return np.random.rand()
    if len(args) == 1:
        args += args
    try:
        return np.random.rand(np.prod(args)).reshape(args,order="F")
    except:
        pass

def randn(*args,**kwargs):
    if not args:
        return np.random.randn()
    if len(args) == 1:
        args += args
    try:
        return np.random.randn(np.prod(args)).reshape(args,order="F")
    except:
        pass

def ravel(a):
    return np.asanyarray(a).reshape(-1,1)

def roots(a):

    return matlabarray(np.roots(np.asarray(a).ravel()))

def round(a):
    return np.round(np.asanyarray(a))

def rows(a):
    return np.asarray(a).shape[0]

def schur(a):
    return matlabarray(_schur(np.asarray(a)))

def size(a, b=0, nargout=1):
    """
    >>> size(zeros(3,3)) + 1
    matlabarray([[4, 4]])
    """
    s = np.asarray(a).shape
    if s is ():
        return 1 if b else (1,)*nargout
    # a is not a scalar
    try:
        if b:
            return s[b-1]
        else:
            return matlabarray(s) if nargout <= 1 else s
    except IndexError:
        return 1

def size_equal(a,b):
    if a.size != b.size:
        return False
    for i in range(len(a.shape)):
        if a.shape[i] != b.shape[i]:
            return False
    return True

from numpy import sqrt
sort = __builtin__.sorted

def strcmp(a,b):
    return str(a) == str(b)

def strread(s, format="", nargout=1):
    if format == "":
        a = [float(x) for x in s.split()]
        return tuple(a) if nargout > 1 else np.asanyarray([a])
    raise NotImplementedError

def strrep(a,b,c):
    return str(a).replace(str(b),str(c))

def sum(a, dim=None):
    if dim is None:
        return np.asanyarray(a).sum()
    else:
        return np.asanyarray(a).sum(dim-1)

def toupper(a):
    return char(str(a.data).upper())

true = True

def tic():
    return time.clock()

def toc(t):
    return time.clock()-t

def true(*args):
    if len(args) == 1:
        args += args
    return matlabarray(np.ones(args,dtype=bool,order="F"))

def version():
    return char('0.26')

def zeros(*args,**kwargs):
    if not args:
        return 0.0
    if len(args) == 1:
        args += args
    return matlabarray(np.zeros(args,**kwargs))

if __name__ == "__main__":
    import doctest
    doctest.testmod()

# vim:et:sw=4:si:tw=60

# SMOP compiler runtime support library
# Copyright 2014 Victor Leikehman

# MIT license
import numpy as np
import os
from scipy.io import loadmat

# abs sort find1 numel rand('seed') sum(a,d) log dot cumsum any(a,d) sqrt proda isinf sign mod

class matlabarray(np.ndarray):
    def __new__(cls,input_array=[],**kwargs):
        #obj = np.array(input_array,ndmin=2,order="F",copy=None).view(cls)
        obj = np.asarray(input_array,order="F").view(cls)
        return obj

    def __array_finalize__(self,obj):
        self.delegate = obj
        
    def compute_indices(self,index):
        if not isinstance(index,tuple):
           index = index,
        if len(index) != 1 and len(index) != self.delegate.ndim:
            raise IndexError()
        indices = []
        for i,ix in enumerate(index):
            if ix.__class__ is slice:
                if len(index) == 1:
                    n = self.delegate.size
                else:
                    n = self.delegate.shape[i]
                start,stop,step = ix.indices(n)
                indices.append(slice(start-1 if start else 0,
                                     stop,
                                     step))
            else:        
                try:
                    indices.append(ix-1)
                except TypeError:
                    indices.append(np.asarray(ix)-1)
        return tuple(indices)

    def __getslice__(self,i,j):
        return self.__getitem__(slice(i,j,None))

    def __getitem__(self,index):
        if index  == slice(None):
            return self.delegate.reshape(-1,1,order="F")
        indices = self.compute_indices(index)
        if len(indices) == 1:
            return self.delegate.reshape(-1,order="F").__getitem__(indices)
        else:
            return self.delegate.__getitem__(indices)

    def __setitem__(self,index,value):
        indices = self.compute_indices(index)
        for j in (1,2):
            try:
                if isinstance(index,tuple):
                    np.ndarray.__setitem__(self.delegate,indices,value)
                else:
                    np.ndarray.__setitem__(self.delegate.reshape(-1,order="F"),indices,value)
                break
            except IndexError:
                if isinstance(index,tuple):
                    new_shape = [max(self.delegate.shape[d], i+1) for d,i in enumerate(indices)]
                    # new_shape = [self.new_size(i) for i in indices]
                else:
                    raise NotImplementedError("Expanding arrays not implemented yet")
                #self.delegate = np.resize(self.delegate,new_shape)
                self.delegate = self.delegate.copy()
                self.delegate.resize(new_shape)

    def reshape_(self,*args):
        return self.delegate.reshape(*args,order="F")

#    def __copy__(self):
#        obj = matlabarray()
#        obj.delegate = self.delegate.copy()
#        return obj

    def __repr__(self):
        return self.delegate.__repr__()

    def __str__(self):
        return self.delegate.__str__()

    def __add__(self,other):
        return np.ndarray.__add__(self,other)

    def __neg__(self):
        return matlabarray(self.delegate.__neg__())

def asarray(a):
    return a.delegate if isinstance(a,matlabarray) else np.asarray(a,order="F")

def abs_(a):
    return np.abs(asarray(a))

def arange_(*args):
#    if args is ():
#        return slice(None,None,None)
#    if len(args) == 1:
#        return np.arange(1,args[0]+1).reshape(-1,1)
#   if len(args) == 2:
        return np.arange(args[0],args[1]+1).reshape(-1,1)
#   if len(args) == 3:
#       return np.arange(args[0],args[1]+1,args[2]).reshape(-1,1)
#   assert 0

def ceil_(a):
    return np.ceil(asarray(a))

def disp_(*args):
    print (args)

def false_(*args):
    if len(args) == 1:
        args += args
    return np.zeros(args,dtype=bool,order="F")

def find_(a,nargout=2):
    i,j = asarray(a).nonzero()
    return (i+1),(j+1)

def floor_(a):
    return np.floor_(asarray(a))

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
        return 0 in asarray(a).shape
    except AttributeError:
        return False

def isequal_(a,b):
    return np.array_equal(asarray(a),
                          asarray(b))
                          

def length_(a):
    try:
        return max(asarray(a).shape)
    except ValueError:
        return 1

def load_(a):
    return loadmat(a) # FIXME

def max_(a, d=0, nargout=0):
    if d or nargout:
        raise ErrorNotImplemented()
    return asarray(a).amax()

def min_(a, d=0, nargout=0):
    if d or nargout:
        raise ErrorNotImplemented()
    return asarray(a).amin()

def numel_(a):
    return asarray(a).size

def ones_(*args,**kwargs):
        return matlabarray(np.ones(args,dtype="int",**kwargs))

def rand_(*args):
    if len(args) == 1:
        args += args
    return np.rand(args,order="F")

def ravel_(a):
    return asarray(a).reshape(-1,1)

def round_(a):
    return np.round(asarray(a))

def size_(a, b=0, nargout=2):
    s = asarray(a).shape
    if s is ():
        return 1 if b else (1,)*nargout
    # a is not a scalar
    try:
        return s[b-1] if b else s
    except IndexError:
        return 1

def strread(s, format="", nargout=1):
    if format == "":
        a = [float(x) for x in s.split()]
        return tuple(a) if nargout > 1 else np.asarray([a])
    raise ErrorNotImplemented

def sum_(a):
    return asarray(a).sum()

def true_(*args):
    if len(args) == 1:
        args += args
    return np.ones(args,dtype=bool,order="F")

def zeros_(*args):
    if not args:
        return 0.0
    if len(args) == 1:
        args += args
    return np.zeros(args,order="F")

# vim:et:sw=4:si:

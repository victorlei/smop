# SMOP compiler runtime support library
# Copyright 2014 Victor Leikehman

# MIT license

import numpy

def ceil_(t):
    '''
    >>> ceil_(-1.1)
    -1.0
    '''
    return numpy.ceil(t)

def false_(*args):
    return numpy.zeros(args,dtype=bool)

def find_(a,nargout=2):
    i,j = a.nonzero()
    return (i+1),(j+1)

def floor_(t):
    '''
    >>> floor_(-0.1)
    -1.0
    '''
    return numpy.floor(t)

def inf_(*args):
    t = numpy.empty(numpy.prod(args))
    t[:] = numpy.inf
    return t.reshape(args)

Inf_ = inf_

def isequal_(a,b):
    return numpy.array_equal(a,b)

def length_(t):
    '''
    >>> length_(zeros_(3,4,5))
    5
    >>> length_(12345)
    1
    >>> length_([3,4,5])
    3
    '''
    return max(numpy.shape(t) or (1,))

def max_(t, d=0, nargout=0):
    '''
    >>> max_(123)
    123
    >>> max_(range(10))
    9
    >>> max_(numpy.arange(10))
    9
    '''
    if d or nargout:
        raise ErrorNotImplemented
    return numpy.amax(t)

def min_(t, d=0, nargout=0):
    '''
    >>> min_(123)
    123
    >>> min_(range(10))
    0
    >>> min_(numpy.arange(10))
    0
    '''
    if d or nargout:
        raise ErrorNotImplemented
    return numpy.amin(t)

def ones_(*args):
        return numpy.ones(args)

def rand_(*args):
    '''
    >>> size_(rand_())
    (1, 1)
    >>> size_(rand_(2))
    (2, 2)
    >>> size_(rand_(2,3))
    (2, 3)
    '''
    if len(args) == 1:
        args += args
    return numpy.random.rand(*args)

def ravel_(t):
    '''
    >>> size_(ravel_(3))
    (1, 1)
    >>> size_(ravel_(zeros_(3,3)))
    (9, 1)
    '''
    return numpy.reshape(t,[-1,1])

def round_(t):
    '''
    >>> round_(0.5)
    0.0
    >>> round_(0.6)
    1.0
    '''
    return numpy.round(t)

def size_(a, b=0, nargout=2):
    '''
    >>> size_(123, 100)
    1
    >>> size_(123)
    (1, 1)
    >>> (p,q,r) = size_(123,nargout=3)
    >>> (p,q,r)
    (1, 1, 1)
    >>> z = zeros_(2,3,4)
    >>> a = size_(z)
    >>> a
    (2, 3, 4)
    >>> b = zeros_(2,3)
    >>> (m,n) = size_(b,nargout=2)
    >>> (m,n)
    (2, 3)
    '''
    s = numpy.shape(a)
    if s is ():
        return 1 if b else (1,)*nargout
    # a is not a scalar
    return s[b-1] if b else s

def true_(*args):
    return numpy.ones(args,dtype=bool)

def zeros_(*args,**kwargs):
    '''
    >>> size_(zeros_(2,3))
    (2, 3)
    '''
    return numpy.zeros(args,**kwargs)

if __name__ == "__main__":
    import doctest
    doctest.testmod() 

# vim:et:sw=4:si:

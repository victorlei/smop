# SMOP compiler runtime support library
# Copyright 2014 Victor Leikehman

# MIT license

import numpy

def floor_(t):
    return numpy.floor(t)

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

def zeros_(*args,**kwargs):
    return numpy.zeros(args,**kwargs)

if __name__ == "__main__":
    import doctest
    doctest.testmod() 

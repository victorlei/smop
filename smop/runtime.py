# SMOP compiler runtime support library
# Copyright 2014 Victor Leikehman

# MIT license

import numpy as np
import __builtin__

def floor(t):
    return numpy.floor(t)

def length(t):
    try:
        return __builtin__.max(t.shape)
    except:
        return 1

def round(t):
    return numpy.round(t)

def size(a,d=None,nargout=1):
    """
    >>> z = np.arange(12).reshape(3,4)
    >>> size(z)
    3 4
    >>> size(z,1)
    3
    >>> size(z,2)
    4
    """
    try:
        if nargout == 1:
            return a.size if d is None else a[d-1]
        return a.shape if a.ndim == 2 else -1
    except AttributeError:
        return [1,1]

if __name__ == "__main__":
    import doctest
    doctest.testmod() 

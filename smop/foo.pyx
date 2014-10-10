# -*- python -*-
import runtime as rt
import numpy as np
cimport numpy as np

cdef class foo:
    cdef int[:,:] data

    def __init__(self, int[:,:] a):
        self.data = a

    def __getitem__(self,tuple index):
        cdef int i = index[0]
        cdef int j = index[1]
        return self.data[i-1,j-1]

a = rt.matlabarray([[1,2,3,4]],dtype=int)
b = foo(a)
a[1,1] = 11

print a[1,1], b[1,1]

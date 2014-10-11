# -*- python -*-
#import runtime as rt
import numpy as np
cimport numpy as np
from runtime import *

cdef class foo:
    cdef int[:,:] data

    def __init__(self, int[:,:] a):
        self.data = a

    def __getitem__(self,tuple index):
        cdef int i = index[0]
        cdef int j = index[1]
        return self.data[i-1,j-1]

cdef class bar:
    cdef double[:,:] data

    def __init__(self, double[:,:] a):
        self.data = a

    def __getitem__(self,tuple index):
        cdef int i = index[0]
        cdef int j = index[1]
        return self.data[i-1,j-1]

cdef tuple find(np.ndarray[char,ndim=2,cast=True] a): # 38
#cdef tuple find(np.ndarray a): 
    cdef int i,j,k
    cdef char* p = a.data
    for k in range(a.size):
        print(k,p[k])
        if p[k]:
            i,j = np.unravel_index(k, (a.shape[0],a.shape[1]), order="F")
            return i+1,j+1
    return 0,0

a = matlabarray([[1,2],[3,4]],dtype=int)
print(a)
b = matlabarray([[1,2],[3,4]],dtype=float)
print(b)
c = foo(a)
d = bar(b)

a[1,1] = 11
b[1,1] = 1.11

print(a[1,1], b[1,1], c[1,1], d[1,1])

i,j = find(a == 1)
print(i,j)

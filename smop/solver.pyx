# cython: profile=True
from __future__ import division
import numpy as np
cimport numpy as np
from runtime import *

cdef extern from "math.h":
    double ceil(double a)

cdef double s1 = 1.0
cdef double s2 = 2.0
cdef double s3 = 3.0

cdef double r8_random():
    global s1, s2, s3
    s1 = 171 * s1 % 30269
    s2 = 172 * s2 % 30307
    s3 = 170 * s3 % 30323
    return (s1 / 30269.0 + s2 / 30307.0 + s3 / 30323.0) % 1.0


#def find(np.ndarray a):
#    cdef int i,j
#    cdef int m = a.shape[0]
#    cdef int n = a.shape[1]
#    
#    for i in range(m):
#        for j in range(n):
#            if a[i,j]:
#                return i+1,j+1
#    else:
#        raise ValueError

cdef tuple find(np.ndarray[char,ndim=2,cast=True] a): # 38
    cdef int i,j,k
    cdef char* p = a.data
    for k in range(a.size):
        if p[k]:
            i,j = np.unravel_index(k, (a.shape[0],a.shape[1]), order="F")
            return i+1,j+1
    raise ValueError

#cdef tuple find(np.ndarray a): # 50
#    cdef int k=0
#    s = (a.shape[0],a.shape[1])
#    for x in np.nditer(a):
#        if x:
#            i,j = np.unravel_index(k,s,order="F")
#            return i+1,j+1
#        k = k+1
#    raise ValueError

def solver_(np.ndarray ai,
            np.ndarray af,
            int w,
            int nargout=1):
    cdef int nBlocks,m,n,i,j,r,ni,nj,ti,tj,d,dn
    cdef np.ndarray mv
    cdef int bid
    #rand_(1,2,3)
    nBlocks=max_(ai[:])
    m,n=size_(ai,nargout=2)
    cdef np.ndarray I = matlabarray([0,1,0,- 1])
    cdef np.ndarray J = matlabarray([1,0,- 1,0])
    cdef np.ndarray a = copy_(ai)
    mv=matlabarray([])
    while not isequal_(af,a):

        bid = int(ceil(r8_random() * nBlocks))
        i,j=find(a == bid)
        r=int(ceil(r8_random() * 4))
        ni=i + I[r]
        nj=j + J[r]
        if (ni < 1) or (ni > m) or (nj < 1) or (nj > n):
            continue
        if a[ni,nj] > 0:
            continue
        ti,tj=find(af == bid)
        d=(ti - i) ** 2 + (tj - j) ** 2
        dn=(ti - ni) ** 2 + (tj - nj) ** 2
        if (d < dn) and (r8_random() > 0.05):
            continue
        a[ni,nj]=bid
        a[i,j]=0
        mv[mv.shape[0] + 1,[1,2]]=[bid,r]

    return mv

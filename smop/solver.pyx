# cython: profile=True
from __future__ import division
import numpy as np
cimport numpy as np
from runtime import *
def solver_(np.ndarray ai,np.ndarray af,int w,int nargout=1):
    cdef int nBlocks,m,n,i,j,r,bid,ni,nj,ti,tj,d,dn
    cdef np.ndarray a,I,J,mv
    #rand_(1,2,3)
    nBlocks=max_(ai[:])
    m,n=size_(ai,nargout=2)
    I=matlabarray([0,1,0,- 1])
    J=matlabarray([1,0,- 1,0])
    a=copy_(ai)
    mv=matlabarray([])
    while not isequal_(af,a):

        bid=ceil_(rand_() * nBlocks)
        i,j=find_(a == bid,nargout=2)
        r=ceil_(rand_() * 4)
        ni=i + I[r]
        nj=j + J[r]
        if (ni < 1) or (ni > m) or (nj < 1) or (nj > n):
            continue
        if a[ni,nj] > 0:
            continue
        ti,tj=find_(af == bid,nargout=2)
        d=(ti - i) ** 2 + (tj - j) ** 2
        dn=(ti - ni) ** 2 + (tj - nj) ** 2
        if (d < dn) and (rand_() > 0.05):
            continue
        a[ni,nj]=bid
        a[i,j]=0
        mv[mv.shape[0] + 1,[1,2]]=[bid,r]

    return mv

s1=1
s2=2
s3=3
cdef double rand_():
    global s1,s2,s3
    r,s1,s2,s3=r8_random_(s1,s2,s3,nargout=4)
    return r

def r8_random_(double s1,double s2,double s3,nargout=1):
    cdef double r
    s1=171 * s1 % 30269
    s2=172 * s2 % 30307
    s3=170 * s3 % 30323
    r=(s1 / 30269.0 + s2 / 30307.0 + s3 / 30323.0) % 1.0
    return r,s1,s2,s3

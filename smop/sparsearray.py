import numpy as np
import sys

class sparsearray(dict):
    def __init__(self,input_array=[[]]):
        #import pdb;pdb.set_trace()
        dict.__init__(self)
        a = np.atleast_2d(input_array)
        self.dtype = a.dtype
        self.ndim = a.ndim
        self._shape = None
        if not a.size:
            return
        it = np.nditer(a, flags=['multi_index'])
        while not it.finished:
            index = tuple([i+1 for i in it.multi_index])
            self.setdefault(index,it[0].item())
            it.iternext()

    @property
    def shape(self):
        #import pdb;pdb.set_trace()
        if self._shape is None:
            s = [0] * self.ndim
            for key in self.keys():
                for i,k in enumerate(key):
                    s[i] = max(k,s[i])
            self._shape = tuple(s)
        return self._shape

    def todense(self):
        a = np.zeros(self.shape,dtype=self.dtype)
        for key,value in self.iteritems():
            key = tuple([i-1 for i in key])
            a.__setitem__(key,value)
        return a

    def __str__(self):
        return str(self.todense())

    def __repr__(self):
        return repr(self.todense())

    def copy(self):
        #return copy.copy(self)
        return self.todense()

    def __setitem__(self,index,value):
        if np.isscalar(value):
            for key in self.iterkeys(index):
                dict.__setitem__(self,key,value)
            self._shape = None
        else:
            raise NotImplementedError

    def __getslice__(self,i,j):
        if j == sys.maxint:
            j = None
        return self.__getitem__(slice(i,j,None))

    def __getitem__(self,index):
        try:
            #a = [dict.__getitem__(self,key) for key in self.iterkeys(index)]
            a = [self.get(key,0) for key in self.iterkeys(index)]
            if len(a) == 1:
                return a[0]
        except ValueError:
            raise IndexError # out of bound rhs indexing
        #return a
        #return sparsearray([a])
        return np.array(a)

    def iterkeys(self,index):
        #import pdb; pdb.set_trace()
        if not isinstance(index,tuple) and self.shape[0] == 1:
            index = (1,index)
        if isinstance(index, int):
            key = np.unravel_index(index-1, self.shape, order='F')
            yield tuple(k+1 for k in key)
        elif isinstance(index,slice):
            index = range((index.start or 1)-1,
                          index.stop or np.prod(self.shape),
                          index.step or 1)
            for key in np.transpose(np.unravel_index(index, self.shape, order='F')): # 0-based
                yield tuple(k+1 for k in key)
        elif isinstance(index,(list,np.ndarray)):
            index = np.asarray(index)-1
            for key in np.transpose(np.unravel_index(index, self.shape, order='F')):
                yield tuple(k+1 for k in key)
        else:
            assert isinstance(index,tuple),index.__class__
            indices = []  # 1-based
            for i,ix in enumerate(index):
                if isinstance(ix,slice):
                    indices.append(np.arange((ix.start or 1),
                                             (ix.stop  or self.shape[i]) + 1,
                                             ix.step   or 1,
                                             dtype=int))
                else:
                    indices.append(np.asarray(ix))
            assert len(index) == 2
            indices[0].shape = (-1,1)
            for key in np.broadcast(*indices):
                yield tuple(map(int,key))


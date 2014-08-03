import numpy
import a
from runtime import *

ai = matlabarray(zeros_(10,10,dtype=int))
af = copy_(ai)

ai[1,1]=2
ai[2,2]=3
ai[3,3]=4
ai[4,4]=5
ai[5,5]=1

af[9,9]=1
af[8,8]=2
af[7,7]=3
af[6,6]=4
af[10,10]=5

mv = a.solver_(ai,af,0)
print mv

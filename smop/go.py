import pstats,cProfile
import numpy,time
import solver as a
from core import *

def main():
    ai = matlabarray(zeros (10,10,dtype=int),dtype=int)
    af = copy(ai)

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

    t0 = time.clock()
    mv = a.solver(ai,af,0)
    t1 = time.clock()
    print t1-t0
    print mv.shape

if __name__ == '__main__':
    main()
    """
    cProfile.runctx('main()',globals(),locals(),"Profile.prof")
    s = pstats.Stats("Profile.prof")
    s.strip_dirs().sort_stats("time").print_stats()
    """

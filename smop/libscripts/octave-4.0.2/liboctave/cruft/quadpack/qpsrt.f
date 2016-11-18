      subroutine qpsrt(limit,last,maxerr,ermax,elist,iord,nrmax)
c***begin prologue  qpsrt
c***refer to  qage,qagie,qagpe,qagse,qawce,qawse,qawoe
c***routines called  (none)
c***keywords  sequential sorting
c***description
c
c 1.        qpsrt
c           ordering routine
c              standard fortran subroutine
c              real version
c
c 2.        purpose
c              this routine maintains the descending ordering
c              in the list of the local error estimates resulting from
c              the interval subdivision process. at each call two error
c              estimates are inserted using the sequential search
c              method, top-down for the largest error estimate
c              and bottom-up for the smallest error estimate.
c
c 3.        calling sequence
c              call qpsrt(limit,last,maxerr,ermax,elist,iord,nrmax)
c
c           parameters (meaning at output)
c              limit  - integer
c                       maximum number of error estimates the list
c                       can contain
c
c              last   - integer
c                       number of error estimates currently
c                       in the list
c
c              maxerr - integer
c                       maxerr points to the nrmax-th largest error
c                       estimate currently in the list
c
c              ermax  - real
c                       nrmax-th largest error estimate
c                       ermax = elist(maxerr)
c
c              elist  - real
c                       vector of dimension last containing
c                       the error estimates
c
c              iord   - integer
c                       vector of dimension last, the first k
c                       elements of which contain pointers
c                       to the error estimates, such that
c                       elist(iord(1)),... , elist(iord(k))
c                       form a decreasing sequence, with
c                       k = last if last.le.(limit/2+2), and
c                       k = limit+1-last otherwise
c
c              nrmax  - integer
c                       maxerr = iord(nrmax)
c
c 4.        no subroutines or functions needed
c***end prologue  qpsrt
c
      real elist,ermax,errmax,errmin
      integer i,ibeg,ido,iord,isucc,j,jbnd,jupbn,k,last,limit,maxerr,
     *  nrmax
      dimension elist(last),iord(last)
c
c           check whether the list contains more than
c           two error estimates.
c
c***first executable statement  qpsrt
      if(last.gt.2) go to 10
      iord(1) = 1
      iord(2) = 2
      go to 90
c
c           this part of the routine is only executed
c           if, due to a difficult integrand, subdivision
c           increased the error estimate. in the normal case
c           the insert procedure should start after the
c           nrmax-th largest error estimate.
c
   10 errmax = elist(maxerr)
      if(nrmax.eq.1) go to 30
      ido = nrmax-1
      do 20 i = 1,ido
        isucc = iord(nrmax-1)
c ***jump out of do-loop
        if(errmax.le.elist(isucc)) go to 30
        iord(nrmax) = isucc
        nrmax = nrmax-1
   20    continue
c
c           compute the number of elements in the list to
c           be maintained in descending order. this number
c           depends on the number of subdivisions still
c           allowed.
c
   30 jupbn = last
      if(last.gt.(limit/2+2)) jupbn = limit+3-last
      errmin = elist(last)
c
c           insert errmax by traversing the list top-down,
c           starting comparison from the element elist(iord(nrmax+1)).
c
      jbnd = jupbn-1
      ibeg = nrmax+1
      if(ibeg.gt.jbnd) go to 50
      do 40 i=ibeg,jbnd
        isucc = iord(i)
c ***jump out of do-loop
        if(errmax.ge.elist(isucc)) go to 60
        iord(i-1) = isucc
   40 continue
   50 iord(jbnd) = maxerr
      iord(jupbn) = last
      go to 90
c
c           insert errmin by traversing the list bottom-up.
c
   60 iord(i-1) = maxerr
      k = jbnd
      do 70 j=i,jbnd
        isucc = iord(k)
c ***jump out of do-loop
        if(errmin.lt.elist(isucc)) go to 80
        iord(k+1) = isucc
        k = k-1
   70 continue
      iord(i) = last
      go to 90
   80 iord(k+1) = last
c
c           set maxerr and ermax.
c
   90 maxerr = iord(nrmax)
      ermax = elist(maxerr)
      return
      end

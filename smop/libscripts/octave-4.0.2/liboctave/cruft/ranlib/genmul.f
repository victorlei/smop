      SUBROUTINE genmul(n,p,ncat,ix)
C**********************************************************************
C
C            SUBROUTINE GENMUL( N, P, NCAT, IX )
C     GENerate an observation from the MULtinomial distribution
C
C
C                              Arguments
C
C
C     N --> Number of events that will be classified into one of
C           the categories 1..NCAT
C                         INTEGER N
C
C     P --> Vector of probabilities.  P(i) is the probability that
C           an event will be classified into category i.  Thus, P(i)
C           must be [0,1]. Only the first NCAT-1 P(i) must be defined
C           since P(NCAT) is 1.0 minus the sum of the first
C           NCAT-1 P(i).
C                         REAL P(NCAT-1)
C
C     NCAT --> Number of categories.  Length of P and IX.
C                         INTEGER NCAT
C
C     IX <-- Observation from multinomial distribution.  All IX(i)
C            will be nonnegative and their sum will be N.
C                         INTEGER IX(NCAT)
C
C
C                              Method
C
C
C     Algorithm from page 559 of
C
C     Devroye, Luc
C
C     Non-Uniform Random Variate Generation.  Springer-Verlag,
C     New York, 1986.
C
C**********************************************************************
C     .. Scalar Arguments ..
      INTEGER n,ncat
C     ..
C     .. Array Arguments ..
      REAL p(*)
      INTEGER ix(*)
C     ..
C     .. Local Scalars ..
      REAL prob,ptot,sum
      INTEGER i,icat,ntot
C     ..
C     .. External Functions ..
      INTEGER ignbin
      EXTERNAL ignbin
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC abs
C     ..
C     .. Executable Statements ..

C     Check Arguments
      IF (n.LT.0) CALL XSTOPX ('N < 0 in GENMUL')
      IF (ncat.LE.1) CALL XSTOPX ('NCAT <= 1 in GENMUL')
      ptot = 0.0
      DO 10,i = 1,ncat - 1
          IF (p(i).LT.0.0) CALL XSTOPX ('Some P(i) < 0 in GENMUL')
          IF (p(i).GT.1.0) CALL XSTOPX ('Some P(i) > 1 in GENMUL')
          ptot = ptot + p(i)
   10 CONTINUE
      IF (ptot.GT.0.99999) CALL XSTOPX ('Sum of P(i) > 1 in GENMUL')

C     Initialize variables
      ntot = n
      sum = 1.0
      DO 20,i = 1,ncat
          ix(i) = 0
   20 CONTINUE

C     Generate the observation
      DO 30,icat = 1,ncat - 1
          prob = p(icat)/sum
          ix(icat) = ignbin(ntot,prob)
          ntot = ntot - ix(icat)
          IF (ntot.LE.0) RETURN
          sum = sum - p(icat)
   30 CONTINUE
      ix(ncat) = ntot

C     Finished
      RETURN

      END

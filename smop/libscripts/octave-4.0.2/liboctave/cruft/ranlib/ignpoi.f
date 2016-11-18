      INTEGER FUNCTION ignpoi(mu)
C**********************************************************************
C
C     INTEGER FUNCTION IGNPOI( MU )
C
C                    GENerate POIsson random deviate
C
C
C                              Function
C
C
C     Generates a single random deviate from a Poisson
C     distribution with mean MU.
C
C
C                              Arguments
C
C
C     MU --> The mean of the Poisson distribution from which
C            a random deviate is to be generated.
C                              REAL MU
C     JJV                    (MU >= 0.0)
C
C     IGNPOI <-- The random deviate.
C                              INTEGER IGNPOI (non-negative)
C
C
C                              Method
C
C
C     Renames KPOIS from TOMS as slightly modified by BWB to use RANF
C     instead of SUNIF.
C
C     For details see:
C
C               Ahrens, J.H. and Dieter, U.
C               Computer Generation of Poisson Deviates
C               From Modified Normal Distributions.
C               ACM Trans. Math. Software, 8, 2
C               (June 1982),163-179
C
C**********************************************************************
C**********************************************************************C
C**********************************************************************C
C                                                                      C
C                                                                      C
C     P O I S S O N  DISTRIBUTION                                      C
C                                                                      C
C                                                                      C
C**********************************************************************C
C**********************************************************************C
C                                                                      C
C     FOR DETAILS SEE:                                                 C
C                                                                      C
C               AHRENS, J.H. AND DIETER, U.                            C
C               COMPUTER GENERATION OF POISSON DEVIATES                C
C               FROM MODIFIED NORMAL DISTRIBUTIONS.                    C
C               ACM TRANS. MATH. SOFTWARE, 8,2 (JUNE 1982), 163 - 179. C
C                                                                      C
C     (SLIGHTLY MODIFIED VERSION OF THE PROGRAM IN THE ABOVE ARTICLE)  C
C                                                                      C
C**********************************************************************C
C
C      INTEGER FUNCTION IGNPOI(IR,MU)
C
C     INPUT:  IR=CURRENT STATE OF BASIC RANDOM NUMBER GENERATOR
C             MU=MEAN MU OF THE POISSON DISTRIBUTION
C     OUTPUT: IGNPOI=SAMPLE FROM THE POISSON-(MU)-DISTRIBUTION
C
C
C
C     MUPREV=PREVIOUS MU, MUOLD=MU AT LAST EXECUTION OF STEP P OR CASE B
C     TABLES: COEFFICIENTS A0-A7 FOR STEP F. FACTORIALS FACT
C     COEFFICIENTS A(K) - FOR PX = FK*V*V*SUM(A(K)*V**K)-DEL
C
C
C
C     SEPARATION OF CASES A AND B
C
C     .. Scalar Arguments ..
      REAL mu
C     ..
C     .. Local Scalars ..
      REAL a0,a1,a2,a3,a4,a5,a6,a7,b1,b2,c,c0,c1,c2,c3,d,del,difmuk,e,
     +     fk,fx,fy,g,muold,muprev,omega,p,p0,px,py,q,s,t,u,v,x,xx
C     JJV I added a variable 'll' here - it is the 'l' for CASE A
      INTEGER j,k,kflag,l,ll,m
C     ..
C     .. Local Arrays ..
      REAL fact(10),pp(35)
C     ..
C     .. External Functions ..
      REAL ranf,sexpo,snorm
      EXTERNAL ranf,sexpo,snorm
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC abs,alog,exp,float,ifix,max0,min0,sign,sqrt
C     ..
C     JJV added this for case: mu unchanged
C     .. Save statement ..
      SAVE s, d, l, ll, omega, c3, c2, c1, c0, c, m, p, q, p0,
     +     a0, a1, a2, a3, a4, a5, a6, a7, fact, pp, muprev, muold
C     ..
C     JJV end addition - I am including vars in Data statements
C     .. Data statements ..
C     JJV changed initial values of MUPREV and MUOLD to -1.0E37
C     JJV if no one calls IGNPOI with MU = -1.0E37 the first time,
C     JJV the code shouldn't break
      DATA muprev,muold/-1.0E37,-1.0E37/
      DATA a0,a1,a2,a3,a4,a5,a6,a7/-.5,.3333333,-.2500068,.2000118,
     +     -.1661269,.1421878,-.1384794,.1250060/
      DATA fact/1.,1.,2.,6.,24.,120.,720.,5040.,40320.,362880./
      DATA pp/35*0.0/
C     ..
C     .. Executable Statements ..

      IF (mu.EQ.muprev) GO TO 10
      IF (mu.LT.10.0) GO TO 120
C
C     C A S E  A. (RECALCULATION OF S,D,LL IF MU HAS CHANGED)
C
C     JJV This is the case where I changed 'l' to 'll'
C     JJV Here 'll' is set once and used in a comparison once

      muprev = mu
      s = sqrt(mu)
      d = 6.0*mu*mu
C
C             THE POISSON PROBABILITIES PK EXCEED THE DISCRETE NORMAL
C             PROBABILITIES FK WHENEVER K >= M(MU). LL=IFIX(MU-1.1484)
C             IS AN UPPER BOUND TO M(MU) FOR ALL MU >= 10 .
C
      ll = ifix(mu-1.1484)
C
C     STEP N. NORMAL SAMPLE - SNORM(IR) FOR STANDARD NORMAL DEVIATE
C
   10 g = mu + s*snorm()
      IF (g.LT.0.0) GO TO 20
      ignpoi = ifix(g)
C
C     STEP I. IMMEDIATE ACCEPTANCE IF IGNPOI IS LARGE ENOUGH
C
      IF (ignpoi.GE.ll) RETURN
C
C     STEP S. SQUEEZE ACCEPTANCE - SUNIF(IR) FOR (0,1)-SAMPLE U
C
      fk = float(ignpoi)
      difmuk = mu - fk
      u = ranf()
      IF (d*u.GE.difmuk*difmuk*difmuk) RETURN
C
C     STEP P. PREPARATIONS FOR STEPS Q AND H.
C             (RECALCULATIONS OF PARAMETERS IF NECESSARY)
C             .3989423=(2*PI)**(-.5)  .416667E-1=1./24.  .1428571=1./7.
C             THE QUANTITIES B1, B2, C3, C2, C1, C0 ARE FOR THE HERMITE
C             APPROXIMATIONS TO THE DISCRETE NORMAL PROBABILITIES FK.
C             C=.1069/MU GUARANTEES MAJORIZATION BY THE 'HAT'-FUNCTION.
C
   20 IF (mu.EQ.muold) GO TO 30
      muold = mu
      omega = .3989423/s
      b1 = .4166667E-1/mu
      b2 = .3*b1*b1
      c3 = .1428571*b1*b2
      c2 = b2 - 15.*c3
      c1 = b1 - 6.*b2 + 45.*c3
      c0 = 1. - b1 + 3.*b2 - 15.*c3
      c = .1069/mu
   30 IF (g.LT.0.0) GO TO 50
C
C             'SUBROUTINE' F IS CALLED (KFLAG=0 FOR CORRECT RETURN)
C
      kflag = 0
      GO TO 70
C
C     STEP Q. QUOTIENT ACCEPTANCE (RARE CASE)
C
   40 IF (fy-u*fy.LE.py*exp(px-fx)) RETURN
C
C     STEP E. EXPONENTIAL SAMPLE - SEXPO(IR) FOR STANDARD EXPONENTIAL
C             DEVIATE E AND SAMPLE T FROM THE LAPLACE 'HAT'
C             (IF T <= -.6744 THEN PK < FK FOR ALL MU >= 10.)
C
   50 e = sexpo()
      u = ranf()
      u = u + u - 1.0
      t = 1.8 + sign(e,u)
      IF (t.LE. (-.6744)) GO TO 50
      ignpoi = ifix(mu+s*t)
      fk = float(ignpoi)
      difmuk = mu - fk
C
C             'SUBROUTINE' F IS CALLED (KFLAG=1 FOR CORRECT RETURN)
C
      kflag = 1
      GO TO 70
C
C     STEP H. HAT ACCEPTANCE (E IS REPEATED ON REJECTION)
C
   60 IF (c*abs(u).GT.py*exp(px+e)-fy*exp(fx+e)) GO TO 50
      RETURN
C
C     STEP F. 'SUBROUTINE' F. CALCULATION OF PX,PY,FX,FY.
C             CASE IGNPOI .LT. 10 USES FACTORIALS FROM TABLE FACT
C
   70 IF (ignpoi.GE.10) GO TO 80
      px = -mu
      py = mu**ignpoi/fact(ignpoi+1)
      GO TO 110
C
C             CASE IGNPOI .GE. 10 USES POLYNOMIAL APPROXIMATION
C             A0-A7 FOR ACCURACY WHEN ADVISABLE
C             .8333333E-1=1./12.  .3989423=(2*PI)**(-.5)
C
   80 del = .8333333E-1/fk
      del = del - 4.8*del*del*del
      v = difmuk/fk
      IF (abs(v).LE.0.25) GO TO 90
      px = fk*alog(1.0+v) - difmuk - del
      GO TO 100

   90 px = fk*v*v* (((((((a7*v+a6)*v+a5)*v+a4)*v+a3)*v+a2)*v+a1)*v+a0) -
     +     del
  100 py = .3989423/sqrt(fk)
  110 x = (0.5-difmuk)/s
      xx = x*x
      fx = -0.5*xx
      fy = omega* (((c3*xx+c2)*xx+c1)*xx+c0)
      IF (kflag.LE.0) GO TO 40
      GO TO 60
C
C     C A S E  B. (START NEW TABLE AND CALCULATE P0 IF NECESSARY)
C
C     JJV changed MUPREV assignment from 0.0 to initial value
  120 muprev = -1.0E37
      IF (mu.EQ.muold) GO TO 130
C     JJV added argument checker here
      IF (mu.GE.0.0) GO TO 125
      WRITE (*,*) 'MU < 0 in IGNPOI - ABORT'
      WRITE (*,*) 'Value of MU: ',mu
      CALL XSTOPX ('MU < 0 in IGNPOI - ABORT')
C     JJV added line label here
 125  muold = mu
      m = max0(1,ifix(mu))
      l = 0
      p = exp(-mu)
      q = p
      p0 = p
C
C     STEP U. UNIFORM SAMPLE FOR INVERSION METHOD
C
  130 u = ranf()
      ignpoi = 0
      IF (u.LE.p0) RETURN
C
C     STEP T. TABLE COMPARISON UNTIL THE END PP(L) OF THE
C             PP-TABLE OF CUMULATIVE POISSON PROBABILITIES
C             (0.458=PP(9) FOR MU=10)
C
      IF (l.EQ.0) GO TO 150
      j = 1
      IF (u.GT.0.458) j = min0(l,m)
      DO 140 k = j,l
          IF (u.LE.pp(k)) GO TO 180
  140 CONTINUE
      IF (l.EQ.35) GO TO 130
C
C     STEP C. CREATION OF NEW POISSON PROBABILITIES P
C             AND THEIR CUMULATIVES Q=PP(K)
C
  150 l = l + 1
      DO 160 k = l,35
          p = p*mu/float(k)
          q = q + p
          pp(k) = q
          IF (u.LE.q) GO TO 170
  160 CONTINUE
      l = 35
      GO TO 130

  170 l = k
  180 ignpoi = k
      RETURN

      END

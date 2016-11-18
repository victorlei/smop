      INTEGER FUNCTION ignbin(n,pp)
C**********************************************************************
C
C     INTEGER FUNCTION IGNBIN( N, PP )
C
C                    GENerate BINomial random deviate
C
C
C                              Function
C
C
C     Generates a single random deviate from a binomial
C     distribution whose number of trials is N and whose
C     probability of an event in each trial is P.
C
C
C                              Arguments
C
C
C     N  --> The number of trials in the binomial distribution
C            from which a random deviate is to be generated.
C                              INTEGER N
C     JJV                      (N >= 0)
C
C     PP --> The probability of an event in each trial of the
C            binomial distribution from which a random deviate
C            is to be generated.
C                              REAL PP
C     JJV                      (0.0 <= pp <= 1.0)
C
C     IGNBIN <-- A random deviate yielding the number of events
C                from N independent trials, each of which has
C                a probability of event P.
C                              INTEGER IGNBIN
C
C
C                              Note
C
C
C     Uses RANF so the value of the seeds, ISEED1 and ISEED2 must be set
C     by a call similar to the following
C          DUM = RANSET( ISEED1, ISEED2 )
C
C
C                              Method
C
C
C     This is algorithm BTPE from:
C
C         Kachitvichyanukul, V. and Schmeiser, B. W.
C
C         Binomial Random Variate Generation.
C         Communications of the ACM, 31, 2
C         (February, 1988) 216.
C
C**********************************************************************
C     SUBROUTINE BTPEC(N,PP,ISEED,JX)
C
C     BINOMIAL RANDOM VARIATE GENERATOR
C     MEAN .LT. 30 -- INVERSE CDF
C       MEAN .GE. 30 -- ALGORITHM BTPE:  ACCEPTANCE-REJECTION VIA
C       FOUR REGION COMPOSITION.  THE FOUR REGIONS ARE A TRIANGLE
C       (SYMMETRIC IN THE CENTER), A PAIR OF PARALLELOGRAMS (ABOVE
C       THE TRIANGLE), AND EXPONENTIAL LEFT AND RIGHT TAILS.
C
C     BTPE REFERS TO BINOMIAL-TRIANGLE-PARALLELOGRAM-EXPONENTIAL.
C     BTPEC REFERS TO BTPE AND "COMBINED."  THUS BTPE IS THE
C       RESEARCH AND BTPEC IS THE IMPLEMENTATION OF A COMPLETE
C       USABLE ALGORITHM.
C     REFERENCE:  VORATAS KACHITVICHYANUKUL AND BRUCE SCHMEISER,
C       "BINOMIAL RANDOM VARIATE GENERATION,"
C       COMMUNICATIONS OF THE ACM, FORTHCOMING
C     WRITTEN:  SEPTEMBER 1980.
C       LAST REVISED:  MAY 1985, JULY 1987
C     REQUIRED SUBPROGRAM:  RAND() -- A UNIFORM (0,1) RANDOM NUMBER
C                           GENERATOR
C     ARGUMENTS
C
C       N : NUMBER OF BERNOULLI TRIALS            (INPUT)
C       PP : PROBABILITY OF SUCCESS IN EACH TRIAL (INPUT)
C       ISEED:  RANDOM NUMBER SEED                (INPUT AND OUTPUT)
C       JX:  RANDOMLY GENERATED OBSERVATION       (OUTPUT)
C
C     VARIABLES
C       PSAVE: VALUE OF PP FROM THE LAST CALL TO BTPEC
C       NSAVE: VALUE OF N FROM THE LAST CALL TO BTPEC
C       XNP:  VALUE OF THE MEAN FROM THE LAST CALL TO BTPEC
C
C       P: PROBABILITY USED IN THE GENERATION PHASE OF BTPEC
C       FFM: TEMPORARY VARIABLE EQUAL TO XNP + P
C       M:  INTEGER VALUE OF THE CURRENT MODE
C       FM:  FLOATING POINT VALUE OF THE CURRENT MODE
C       XNPQ: TEMPORARY VARIABLE USED IN SETUP AND SQUEEZING STEPS
C       P1:  AREA OF THE TRIANGLE
C       C:  HEIGHT OF THE PARALLELOGRAMS
C       XM:  CENTER OF THE TRIANGLE
C       XL:  LEFT END OF THE TRIANGLE
C       XR:  RIGHT END OF THE TRIANGLE
C       AL:  TEMPORARY VARIABLE
C       XLL:  RATE FOR THE LEFT EXPONENTIAL TAIL
C       XLR:  RATE FOR THE RIGHT EXPONENTIAL TAIL
C       P2:  AREA OF THE PARALLELOGRAMS
C       P3:  AREA OF THE LEFT EXPONENTIAL TAIL
C       P4:  AREA OF THE RIGHT EXPONENTIAL TAIL
C       U:  A U(0,P4) RANDOM VARIATE USED FIRST TO SELECT ONE OF THE
C           FOUR REGIONS AND THEN CONDITIONALLY TO GENERATE A VALUE
C           FROM THE REGION
C       V:  A U(0,1) RANDOM NUMBER USED TO GENERATE THE RANDOM VALUE
C           (REGION 1) OR TRANSFORMED INTO THE VARIATE TO ACCEPT OR
C           REJECT THE CANDIDATE VALUE
C       IX:  INTEGER CANDIDATE VALUE
C       X:  PRELIMINARY CONTINUOUS CANDIDATE VALUE IN REGION 2 LOGIC
C           AND A FLOATING POINT IX IN THE ACCEPT/REJECT LOGIC
C       K:  ABSOLUTE VALUE OF (IX-M)
C       F:  THE HEIGHT OF THE SCALED DENSITY FUNCTION USED IN THE
C           ACCEPT/REJECT DECISION WHEN BOTH M AND IX ARE SMALL
C           ALSO USED IN THE INVERSE TRANSFORMATION
C       R: THE RATIO P/Q
C       G: CONSTANT USED IN CALCULATION OF PROBABILITY
C       MP:  MODE PLUS ONE, THE LOWER INDEX FOR EXPLICIT CALCULATION
C            OF F WHEN IX IS GREATER THAN M
C       IX1:  CANDIDATE VALUE PLUS ONE, THE LOWER INDEX FOR EXPLICIT
C             CALCULATION OF F WHEN IX IS LESS THAN M
C       I:  INDEX FOR EXPLICIT CALCULATION OF F FOR BTPE
C       AMAXP: MAXIMUM ERROR OF THE LOGARITHM OF NORMAL BOUND
C       YNORM: LOGARITHM OF NORMAL BOUND
C       ALV:  NATURAL LOGARITHM OF THE ACCEPT/REJECT VARIATE V
C
C       X1,F1,Z,W,Z2,X2,F2, AND W2 ARE TEMPORARY VARIABLES TO BE
C       USED IN THE FINAL ACCEPT/REJECT TEST
C
C       QN: PROBABILITY OF NO SUCCESS IN N TRIALS
C
C     REMARK
C       IX AND JX COULD LOGICALLY BE THE SAME VARIABLE, WHICH WOULD
C       SAVE A MEMORY POSITION AND A LINE OF CODE.  HOWEVER, SOME
C       COMPILERS (E.G.,CDC MNF) OPTIMIZE BETTER WHEN THE ARGUMENTS
C       ARE NOT INVOLVED.
C
C     ISEED NEEDS TO BE DOUBLE PRECISION IF THE IMSL ROUTINE
C     GGUBFS IS USED TO GENERATE UNIFORM RANDOM NUMBER, OTHERWISE
C     TYPE OF ISEED SHOULD BE DICTATED BY THE UNIFORM GENERATOR
C
C**********************************************************************

C
C
C
C*****DETERMINE APPROPRIATE ALGORITHM AND WHETHER SETUP IS NECESSARY
C
C     ..
C     .. Scalar Arguments ..
      REAL pp
      INTEGER n
C     ..
C     .. Local Scalars ..
      REAL al,alv,amaxp,c,f,f1,f2,ffm,fm,g,p,p1,p2,p3,p4,psave,q,qn,r,u,
     +     v,w,w2,x,x1,x2,xl,xll,xlr,xm,xnp,xnpq,xr,ynorm,z,z2
      INTEGER i,ix,ix1,k,m,mp,nsave
C     ..
C     .. External Functions ..
      REAL ranf
      EXTERNAL ranf
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC abs,alog,amin1,iabs,int,sqrt
C     JJV ..
C     JJV .. Save statement ..
      SAVE p,q,m,fm,xnp,xnpq,p1,xm,xl,xr,c,xll,xlr,p2,p3,p4,qn,r,g,
     +     psave,nsave
C     JJV I am including the variables in data statements
C     ..
C     .. Data statements ..
C     JJV made these ridiculous starting values - the hope is that
C     JJV no one will call this the first time with them as args
      DATA psave,nsave/-1.0E37,-214748365/
C     ..
C     .. Executable Statements ..
      IF (pp.NE.psave) GO TO 10
      IF (n.NE.nsave) GO TO 20
      IF (xnp-30.0.LT.0.0) GO TO 150
      GO TO 30
C
C*****SETUP, PERFORM ONLY WHEN PARAMETERS CHANGE
C

C     JJV added the argument checker - involved only renaming 10
C     JJV and 20 to the checkers and adding checkers
C     JJV Only remaining problem - if called initially with the
C     JJV initial values of psave and nsave, it will hang
 10   IF (pp.LT.0.0) CALL XSTOPX ('PP < 0.0 in IGNBIN - ABORT!')
      IF (pp.GT.1.0) CALL XSTOPX ('PP > 1.0 in IGNBIN - ABORT!')
      psave = pp
      p = amin1(psave,1.-psave)
      q = 1. - p
 20   IF (n.LT.0) CALL XSTOPX ('N < 0 in IGNBIN - ABORT!')
      xnp = n*p
      nsave = n
      IF (xnp.LT.30.) GO TO 140
      ffm = xnp + p
      m = ffm
      fm = m
      xnpq = xnp*q
      p1 = int(2.195*sqrt(xnpq)-4.6*q) + 0.5
      xm = fm + 0.5
      xl = xm - p1
      xr = xm + p1
      c = 0.134 + 20.5/ (15.3+fm)
      al = (ffm-xl)/ (ffm-xl*p)
      xll = al* (1.+.5*al)
      al = (xr-ffm)/ (xr*q)
      xlr = al* (1.+.5*al)
      p2 = p1* (1.+c+c)
      p3 = p2 + c/xll
      p4 = p3 + c/xlr
C      WRITE(6,100) N,P,P1,P2,P3,P4,XL,XR,XM,FM
C  100 FORMAT(I15,4F18.7/5F18.7)
C
C*****GENERATE VARIATE
C
   30 u = ranf()*p4
      v = ranf()
C
C     TRIANGULAR REGION
C
      IF (u.GT.p1) GO TO 40
      ix = xm - p1*v + u
      GO TO 170
C
C     PARALLELOGRAM REGION
C
   40 IF (u.GT.p2) GO TO 50
      x = xl + (u-p1)/c
      v = v*c + 1. - abs(xm-x)/p1
      IF (v.GT.1. .OR. v.LE.0.) GO TO 30
      ix = x
      GO TO 70
C
C     LEFT TAIL
C
   50 IF (u.GT.p3) GO TO 60
      ix = xl + alog(v)/xll
      IF (ix.LT.0) GO TO 30
      v = v* (u-p2)*xll
      GO TO 70
C
C     RIGHT TAIL
C
   60 ix = xr - alog(v)/xlr
      IF (ix.GT.n) GO TO 30
      v = v* (u-p3)*xlr
C
C*****DETERMINE APPROPRIATE WAY TO PERFORM ACCEPT/REJECT TEST
C
   70 k = iabs(ix-m)
      IF (k.GT.20 .AND. k.LT.xnpq/2-1) GO TO 130
C
C     EXPLICIT EVALUATION
C
      f = 1.0
      r = p/q
      g = (n+1)*r
      IF (m-ix.LT.0) GO TO 80
      IF (m-ix.EQ.0) GO TO 120
      GO TO 100
   80 mp = m + 1
      DO 90 i = mp,ix
          f = f* (g/i-r)
   90 CONTINUE
      GO TO 120

  100 ix1 = ix + 1
      DO 110 i = ix1,m
          f = f/ (g/i-r)
  110 CONTINUE
  120 IF (v-f.LE.0) GO TO 170
      GO TO 30
C
C     SQUEEZING USING UPPER AND LOWER BOUNDS ON ALOG(F(X))
C
  130 amaxp = (k/xnpq)* ((k* (k/3.+.625)+.1666666666666)/xnpq+.5)
      ynorm = -k*k/ (2.*xnpq)
      alv = alog(v)
      IF (alv.LT.ynorm-amaxp) GO TO 170
      IF (alv.GT.ynorm+amaxp) GO TO 30
C
C     STIRLING'S FORMULA TO MACHINE ACCURACY FOR
C     THE FINAL ACCEPTANCE/REJECTION TEST
C
      x1 = ix + 1
      f1 = fm + 1.
      z = n + 1 - fm
      w = n - ix + 1.
      z2 = z*z
      x2 = x1*x1
      f2 = f1*f1
      w2 = w*w
      IF (alv- (xm*alog(f1/x1)+ (n-m+.5)*alog(z/w)+ (ix-
     +    m)*alog(w*p/ (x1*q))+ (13860.- (462.- (132.- (99.-
     +    140./f2)/f2)/f2)/f2)/f1/166320.+ (13860.- (462.- (132.- (99.-
     +    140./z2)/z2)/z2)/z2)/z/166320.+ (13860.- (462.- (132.- (99.-
     +    140./x2)/x2)/x2)/x2)/x1/166320.+ (13860.- (462.- (132.- (99.-
     +    140./w2)/w2)/w2)/w2)/w/166320.) .LE. 0.) GO TO 170
      GO TO 30
C
C     INVERSE CDF LOGIC FOR MEAN LESS THAN 30
C
  140 qn = q**n
      r = p/q
      g = r* (n+1)
  150 ix = 0
      f = qn
      u = ranf()
  160 IF (u.LT.f) GO TO 170
      IF (ix.GT.110) GO TO 150
      u = u - f
      ix = ix + 1
      f = f* (g/ix-r)
      GO TO 160

  170 IF (psave.GT.0.5) ix = n - ix
      ignbin = ix
      RETURN

      END

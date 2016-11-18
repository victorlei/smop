      REAL FUNCTION genbet(aa,bb)
C**********************************************************************
C
C     REAL FUNCTION GENBET( A, B )
C               GeNerate BETa random deviate
C
C
C                              Function
C
C
C     Returns a single random deviate from the beta distribution with
C     parameters A and B.  The density of the beta is
C               x^(a-1) * (1-x)^(b-1) / B(a,b) for 0 < x < 1
C
C
C                              Arguments
C
C
C     A --> First parameter of the beta distribution
C                         REAL A
C     JJV                 (A > 1.0E-37)
C
C     B --> Second parameter of the beta distribution
C                         REAL B
C     JJV                 (B > 1.0E-37)
C
C
C                              Method
C
C
C     R. C. H. Cheng
C     Generating Beta Variates with Nonintegral Shape Parameters
C     Communications of the ACM, 21:317-322  (1978)
C     (Algorithms BB and BC)
C
C**********************************************************************
C     .. Parameters ..
C     Close to the largest number that can be exponentiated
      REAL expmax
C     JJV changed this - 89 was too high, and LOG(1.0E38) = 87.49823
      PARAMETER (expmax=87.49823)
C     Close to the largest representable single precision number
      REAL infnty
      PARAMETER (infnty=1.0E38)
C     JJV added the parameter minlog
C     Close to the smallest number of which a LOG can be taken.
      REAL minlog
      PARAMETER (minlog=1.0E-37)
C     ..
C     .. Scalar Arguments ..
      REAL aa,bb
C     ..
C     .. Local Scalars ..
      REAL a,alpha,b,beta,delta,gamma,k1,k2,olda,oldb,r,s,t,u1,u2,v,w,y,
     +     z
      LOGICAL qsame
C     ..
C     .. External Functions ..
      REAL ranf
      EXTERNAL ranf
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC exp,log,max,min,sqrt
C     ..
C     .. Save statement ..
C     JJV added a,b
      SAVE olda,oldb,alpha,beta,gamma,k1,k2,a,b
C     ..
C     .. Data statements ..
C     JJV changed these to ridiculous values
      DATA olda,oldb/-1.0E37,-1.0E37/
C     ..
C     .. Executable Statements ..
      qsame = (olda.EQ.aa) .AND. (oldb.EQ.bb)
      IF (qsame) GO TO 20
C     JJV added small minimum for small log problem in calc of W
      IF (.NOT. (aa.LT.minlog.OR.bb.LT.minlog)) GO TO 10
      WRITE (*,*) ' AA or BB < ',minlog,' in GENBET - Abort!'
      WRITE (*,*) ' AA: ',aa,' BB ',bb
      CALL XSTOPX (' AA or BB too small in GENBET - Abort!')

   10 olda = aa
      oldb = bb
   20 IF (.NOT. (min(aa,bb).GT.1.0)) GO TO 100


C     Alborithm BB

C
C     Initialize
C
      IF (qsame) GO TO 30
      a = min(aa,bb)
      b = max(aa,bb)
      alpha = a + b
      beta = sqrt((alpha-2.0)/ (2.0*a*b-alpha))
      gamma = a + 1.0/beta
   30 CONTINUE
   40 u1 = ranf()
C
C     Step 1
C
      u2 = ranf()
      v = beta*log(u1/ (1.0-u1))
C     JJV altered this
      IF (v.GT.expmax) GO TO 55
C     JJV added checker to see if a*exp(v) will overflow
C     JJV 50 _was_ w = a*exp(v); also note here a > 1.0
   50 w = exp(v)
      IF (w.GT.infnty/a) GO TO 55
      w = a*w
      GO TO 60
 55   w = infnty

   60 z = u1**2*u2
      r = gamma*v - 1.3862944
      s = a + r - w
C
C     Step 2
C
      IF ((s+2.609438).GE. (5.0*z)) GO TO 70
C
C     Step 3
C
      t = log(z)
      IF (s.GT.t) GO TO 70
C
C     Step 4
C
C     JJV added checker to see if log(alpha/(b+w)) will
C     JJV overflow.  If so, we count the log as -INF, and
C     JJV consequently evaluate conditional as true, i.e.
C     JJV the algorithm rejects the trial and starts over
C     JJV May not need this here since ALPHA > 2.0
      IF (alpha/(b+w).LT.minlog) GO TO 40

      IF ((r+alpha*log(alpha/ (b+w))).LT.t) GO TO 40
C
C     Step 5
C
   70 IF (.NOT. (aa.EQ.a)) GO TO 80
      genbet = w/ (b+w)
      GO TO 90

   80 genbet = b/ (b+w)
   90 GO TO 230


C     Algorithm BC

C
C     Initialize
C
  100 IF (qsame) GO TO 110
      a = max(aa,bb)
      b = min(aa,bb)
      alpha = a + b
      beta = 1.0/b
      delta = 1.0 + a - b
      k1 = delta* (0.0138889+0.0416667*b)/ (a*beta-0.777778)
      k2 = 0.25 + (0.5+0.25/delta)*b
  110 CONTINUE
  120 u1 = ranf()
C
C     Step 1
C
      u2 = ranf()
      IF (u1.GE.0.5) GO TO 130
C
C     Step 2
C
      y = u1*u2
      z = u1*y
      IF ((0.25*u2+z-y).GE.k1) GO TO 120
      GO TO 170
C
C     Step 3
C
  130 z = u1**2*u2
      IF (.NOT. (z.LE.0.25)) GO TO 160
      v = beta*log(u1/ (1.0-u1))

C     JJV instead of checking v > expmax at top, I will check
C     JJV if a < 1, then check the appropriate values

      IF (a.GT.1.0) GO TO 135
C     JJV A < 1 so it can help out if EXP(V) would overflow
      IF (v.GT.expmax) GO TO 132
      w = a*exp(v)
      GO TO 200
 132  w = v + log(a)
      IF (w.GT.expmax) GO TO 140
      w = exp(w)
      GO TO 200

C     JJV in this case A > 1
 135  IF (v.GT.expmax) GO TO 140
      w = exp(v)
      IF (w.GT.infnty/a) GO TO 140
      w = a*w
      GO TO 200
 140  w = infnty
      GO TO 200

  160 IF (z.GE.k2) GO TO 120
C
C     Step 4
C
C
C     Step 5
C
  170 v = beta*log(u1/ (1.0-u1))

C     JJV same kind of checking as above
      IF (a.GT.1.0) GO TO 175
C     JJV A < 1 so it can help out if EXP(V) would overflow
      IF (v.GT.expmax) GO TO 172
      w = a*exp(v)
      GO TO 190
 172  w = v + log(a)
      IF (w.GT.expmax) GO TO 180
      w = exp(w)
      GO TO 190

C     JJV in this case A > 1
 175  IF (v.GT.expmax) GO TO 180
      w = exp(v)
      IF (w.GT.infnty/a) GO TO 180
      w = a*w
      GO TO 190

  180 w = infnty

C     JJV here we also check to see if log overlows; if so, we treat it
C     JJV as -INF, which means condition is true, i.e. restart
  190 IF (alpha/(b+w).LT.minlog) GO TO 120
      IF ((alpha* (log(alpha/ (b+w))+v)-1.3862944).LT.log(z)) GO TO 120
C
C     Step 6
C
  200 IF (.NOT. (a.EQ.aa)) GO TO 210
      genbet = w/ (b+w)
      GO TO 220

  210 genbet = b/ (b+w)
  220 CONTINUE
  230 RETURN

      END

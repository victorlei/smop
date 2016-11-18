      REAL FUNCTION sgamma(a)
C**********************************************************************C
C                                                                      C
C                                                                      C
C     (STANDARD-)  G A M M A  DISTRIBUTION                             C
C                                                                      C
C                                                                      C
C**********************************************************************C
C**********************************************************************C
C                                                                      C
C               PARAMETER  A >= 1.0  !                                 C
C                                                                      C
C**********************************************************************C
C                                                                      C
C     FOR DETAILS SEE:                                                 C
C                                                                      C
C               AHRENS, J.H. AND DIETER, U.                            C
C               GENERATING GAMMA VARIATES BY A                         C
C               MODIFIED REJECTION TECHNIQUE.                          C
C               COMM. ACM, 25,1 (JAN. 1982), 47 - 54.                  C
C                                                                      C
C     STEP NUMBERS CORRESPOND TO ALGORITHM 'GD' IN THE ABOVE PAPER     C
C                                 (STRAIGHTFORWARD IMPLEMENTATION)     C
C                                                                      C
C     Modified by Barry W. Brown, Feb 3, 1988 to use RANF instead of   C
C     SUNIF.  The argument IR thus goes away.                          C
C                                                                      C
C**********************************************************************C
C                                                                      C
C               PARAMETER  0.0 < A < 1.0  !                            C
C                                                                      C
C**********************************************************************C
C                                                                      C
C     FOR DETAILS SEE:                                                 C
C                                                                      C
C               AHRENS, J.H. AND DIETER, U.                            C
C               COMPUTER METHODS FOR SAMPLING FROM GAMMA,              C
C               BETA, POISSON AND BINOMIAL DISTRIBUTIONS.              C
C               COMPUTING, 12 (1974), 223 - 246.                       C
C                                                                      C
C     (ADAPTED IMPLEMENTATION OF ALGORITHM 'GS' IN THE ABOVE PAPER)    C
C                                                                      C
C**********************************************************************C
C
C
C     INPUT: A =PARAMETER (MEAN) OF THE STANDARD GAMMA DISTRIBUTION
C     OUTPUT: SGAMMA = SAMPLE FROM THE GAMMA-(A)-DISTRIBUTION
C
C     COEFFICIENTS Q(K) - FOR Q0 = SUM(Q(K)*A**(-K))
C     COEFFICIENTS A(K) - FOR Q = Q0+(T*T/2)*SUM(A(K)*V**K)
C     COEFFICIENTS E(K) - FOR EXP(Q)-1 = SUM(E(K)*Q**K)
C
C     .. Scalar Arguments ..
      REAL a
C     ..
C     .. Local Scalars .. (JJV added B0 to fix rare and subtle bug)
      REAL a1,a2,a3,a4,a5,a6,a7,aa,aaa,b,b0,c,d,e,e1,e2,e3,e4,e5,p,q,q0,
     +     q1,q2,q3,q4,q5,q6,q7,r,s,s2,si,sqrt32,t,u,v,w,x
C     ..
C     .. External Functions ..
      REAL ranf,sexpo,snorm
      EXTERNAL ranf,sexpo,snorm
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC abs,alog,exp,sign,sqrt
C     ..
C     .. Save statement ..
C     JJV added Save statement for vars in Data satatements
      SAVE aa,aaa,s2,s,d,q0,b,si,c,q1,q2,q3,q4,q5,q6,q7,a1,a2,a3,a4,a5,
     +     a6,a7,e1,e2,e3,e4,e5,sqrt32
C     ..
C     .. Data statements ..
C
C     PREVIOUS A PRE-SET TO ZERO - AA IS A', AAA IS A"
C     SQRT32 IS THE SQUAREROOT OF 32 = 5.656854249492380
C
      DATA q1,q2,q3,q4,q5,q6,q7/.04166669,.02083148,.00801191,.00144121,
     +     -.00007388,.00024511,.00024240/
      DATA a1,a2,a3,a4,a5,a6,a7/.3333333,-.2500030,.2000062,-.1662921,
     +     .1423657,-.1367177,.1233795/
      DATA e1,e2,e3,e4,e5/1.,.4999897,.1668290,.0407753,.0102930/
      DATA aa/0.0/,aaa/0.0/,sqrt32/5.656854/
C     ..
C     .. Executable Statements ..
C
      IF (a.EQ.aa) GO TO 10
      IF (a.LT.1.0) GO TO 130
C
C     STEP  1:  RECALCULATIONS OF S2,S,D IF A HAS CHANGED
C
      aa = a
      s2 = a - 0.5
      s = sqrt(s2)
      d = sqrt32 - 12.0*s
C
C     STEP  2:  T=STANDARD NORMAL DEVIATE,
C               X=(S,1/2)-NORMAL DEVIATE.
C               IMMEDIATE ACCEPTANCE (I)
C
   10 t = snorm()
      x = s + 0.5*t
      sgamma = x*x
      IF (t.GE.0.0) RETURN
C
C     STEP  3:  U= 0,1 -UNIFORM SAMPLE. SQUEEZE ACCEPTANCE (S)
C
      u = ranf()
      IF (d*u.LE.t*t*t) RETURN
C
C     STEP  4:  RECALCULATIONS OF Q0,B,SI,C IF NECESSARY
C
      IF (a.EQ.aaa) GO TO 40
      aaa = a
      r = 1.0/a
      q0 = ((((((q7*r+q6)*r+q5)*r+q4)*r+q3)*r+q2)*r+q1)*r
C
C               APPROXIMATION DEPENDING ON SIZE OF PARAMETER A
C               THE CONSTANTS IN THE EXPRESSIONS FOR B, SI AND
C               C WERE ESTABLISHED BY NUMERICAL EXPERIMENTS
C
      IF (a.LE.3.686) GO TO 30
      IF (a.LE.13.022) GO TO 20
C
C               CASE 3:  A .GT. 13.022
C
      b = 1.77
      si = .75
      c = .1515/s
      GO TO 40
C
C               CASE 2:  3.686 .LT. A .LE. 13.022
C
   20 b = 1.654 + .0076*s2
      si = 1.68/s + .275
      c = .062/s + .024
      GO TO 40
C
C               CASE 1:  A .LE. 3.686
C
   30 b = .463 + s + .178*s2
      si = 1.235
      c = .195/s - .079 + .16*s
C
C     STEP  5:  NO QUOTIENT TEST IF X NOT POSITIVE
C
   40 IF (x.LE.0.0) GO TO 70
C
C     STEP  6:  CALCULATION OF V AND QUOTIENT Q
C
      v = t/ (s+s)
      IF (abs(v).LE.0.25) GO TO 50
      q = q0 - s*t + 0.25*t*t + (s2+s2)*alog(1.0+v)
      GO TO 60

   50 q = q0 + 0.5*t*t* ((((((a7*v+a6)*v+a5)*v+a4)*v+a3)*v+a2)*v+a1)*v
C
C     STEP  7:  QUOTIENT ACCEPTANCE (Q)
C
   60 IF (alog(1.0-u).LE.q) RETURN
C
C     STEP  8:  E=STANDARD EXPONENTIAL DEVIATE
C               U= 0,1 -UNIFORM DEVIATE
C               T=(B,SI)-DOUBLE EXPONENTIAL (LAPLACE) SAMPLE
C
   70 e = sexpo()
      u = ranf()
      u = u + u - 1.0
      t = b + sign(si*e,u)
C
C     STEP  9:  REJECTION IF T .LT. TAU(1) = -.71874483771719
C
   80 IF (t.LT. (-.7187449)) GO TO 70
C
C     STEP 10:  CALCULATION OF V AND QUOTIENT Q
C
      v = t/ (s+s)
      IF (abs(v).LE.0.25) GO TO 90
      q = q0 - s*t + 0.25*t*t + (s2+s2)*alog(1.0+v)
      GO TO 100

   90 q = q0 + 0.5*t*t* ((((((a7*v+a6)*v+a5)*v+a4)*v+a3)*v+a2)*v+a1)*v
C
C     STEP 11:  HAT ACCEPTANCE (H) (IF Q NOT POSITIVE GO TO STEP 8)
C
  100 IF (q.LE.0.0) GO TO 70
      IF (q.LE.0.5) GO TO 110
C
C     JJV modified the code through line 125 to handle large Q case
C
      IF (q.LT.15.0) GO TO 105
C
C     JJV Here Q is large enough that Q = log(exp(Q) - 1.0) (for real Q)
C     JJV so reformulate test at 120 in terms of one EXP, if not too big
C     JJV 87.49823 is close to the largest real which can be
C     JJV exponentiated (87.49823 = log(1.0E38))
C
      IF ((q+e-0.5*t*t).GT.87.49823) GO TO 125
      IF (c*abs(u).GT.exp(q+e-0.5*t*t)) GO TO 70
      GO TO 125

 105  w = exp(q) - 1.0
      GO TO 120

  110 w = ((((e5*q+e4)*q+e3)*q+e2)*q+e1)*q
C
C               IF T IS REJECTED, SAMPLE AGAIN AT STEP 8
C
  120 IF (c*abs(u).GT.w*exp(e-0.5*t*t)) GO TO 70
 125  x = s + 0.5*t
      sgamma = x*x
      RETURN
C
C     ALTERNATE METHOD FOR PARAMETERS A BELOW 1  (.3678794=EXP(-1.))
C
C     JJV changed B to B0 (which was added to declarations for this)
C     JJV in 130 to END to fix rare and subtle bug.
C     JJV Line: '130 aa = 0.0' was removed (unnecessary, wasteful).
C     JJV Reasons: the state of AA only serves to tell the A .GE. 1.0
C     JJV case if certain A-dependant constants need to be recalculated.
C     JJV The A .LT. 1.0 case (here) no longer changes any of these, and
C     JJV the recalculation of B (which used to change with an
C     JJV A .LT. 1.0 call) is governed by the state of AAA anyway.
C
 130  b0 = 1.0 + .3678794*a
  140 p = b0*ranf()
      IF (p.GE.1.0) GO TO 150
      sgamma = exp(alog(p)/a)
      IF (sexpo().LT.sgamma) GO TO 140
      RETURN

  150 sgamma = -alog((b0-p)/a)
      IF (sexpo().LT. (1.0-a)*alog(sgamma)) GO TO 140
      RETURN

      END

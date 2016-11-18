      SUBROUTINE CUNK2(Z, FNU, KODE, MR, N, Y, NZ, TOL, ELIM, ALIM)
C***BEGIN PROLOGUE  CUNK2
C***REFER TO  CBESK
C
C     CUNK2 COMPUTES K(FNU,Z) AND ITS ANALYTIC CONTINUATION FROM THE
C     RIGHT HALF PLANE TO THE LEFT HALF PLANE BY MEANS OF THE
C     UNIFORM ASYMPTOTIC EXPANSIONS FOR H(KIND,FNU,ZN) AND J(FNU,ZN)
C     WHERE ZN IS IN THE RIGHT HALF PLANE, KIND=(3-MR)/2, MR=+1 OR
C     -1. HERE ZN=ZR*I OR -ZR*I WHERE ZR=Z IF Z IS IN THE RIGHT
C     HALF PLANE OR ZR=-Z IF Z IS IN THE LEFT HALF PLANE. MR INDIC-
C     ATES THE DIRECTION OF ROTATION FOR ANALYTIC CONTINUATION.
C     NZ=-1 MEANS AN OVERFLOW WILL OCCUR
C
C***ROUTINES CALLED  CAIRY,CS1S2,CUCHK,CUNHJ,R1MACH
C***END PROLOGUE  CUNK2
      COMPLEX AI, ARG, ASUM, BSUM, CFN, CI, CIP,
     * CK, CONE, CRSC, CR1, CR2, CS, CSCL, CSGN, CSPN, CSR, CSS, CY,
     * CZERO, C1, C2, DAI, PHI,  RZ, S1, S2, Y, Z, ZB, ZETA1,
     * ZETA2, ZN, ZR, PHID, ARGD, ZETA1D, ZETA2D, ASUMD, BSUMD
      REAL AARG, AIC, ALIM, ANG, APHI, ASC, ASCLE, BRY, CAR, CPN, C2I,
     * C2M, C2R, ELIM, FMR, FN, FNF, FNU, HPI, PI, RS1, SAR, SGN, SPN,
     * TOL, X, YY, R1MACH
      INTEGER I, IB, IFLAG, IFN, IL, IN, INU, IUF, K, KDFLG, KFLAG, KK,
     * KODE, MR, N, NAI, NDAI, NW, NZ, IDUM, J, IPARD, IC
      DIMENSION BRY(3), Y(N), ASUM(2), BSUM(2), PHI(2), ARG(2),
     * ZETA1(2), ZETA2(2), CY(2), CIP(4), CSS(3), CSR(3)
      DATA CZERO, CONE, CI, CR1, CR2 /
     1         (0.0E0,0.0E0),(1.0E0,0.0E0),(0.0E0,1.0E0),
     1(1.0E0,1.73205080756887729E0),(-0.5E0,-8.66025403784438647E-01)/
      DATA HPI, PI, AIC /
     1     1.57079632679489662E+00,     3.14159265358979324E+00,
     1     1.26551212348464539E+00/
      DATA CIP(1),CIP(2),CIP(3),CIP(4)/
     1 (1.0E0,0.0E0), (0.0E0,-1.0E0), (-1.0E0,0.0E0), (0.0E0,1.0E0)/
C
      KDFLG = 1
      NZ = 0
C-----------------------------------------------------------------------
C     EXP(-ALIM)=EXP(-ELIM)/TOL=APPROX. ONE PRECISION GREATER THAN
C     THE UNDERFLOW LIMIT
C-----------------------------------------------------------------------
      CSCL = CMPLX(1.0E0/TOL,0.0E0)
      CRSC = CMPLX(TOL,0.0E0)
      CSS(1) = CSCL
      CSS(2) = CONE
      CSS(3) = CRSC
      CSR(1) = CRSC
      CSR(2) = CONE
      CSR(3) = CSCL
      BRY(1) = 1.0E+3*R1MACH(1)/TOL
      BRY(2) = 1.0E0/BRY(1)
      BRY(3) = R1MACH(2)
      X = REAL(Z)
      ZR = Z
      IF (X.LT.0.0E0) ZR = -Z
      YY = AIMAG(ZR)
      ZN = -ZR*CI
      ZB = ZR
      INU = INT(FNU)
      FNF = FNU - FLOAT(INU)
      ANG = -HPI*FNF
      CAR = COS(ANG)
      SAR = SIN(ANG)
      CPN = -HPI*CAR
      SPN = -HPI*SAR
      C2 = CMPLX(-SPN,CPN)
      KK = MOD(INU,4) + 1
      CS = CR1*C2*CIP(KK)
      IF (YY.GT.0.0E0) GO TO 10
      ZN = CONJG(-ZN)
      ZB = CONJG(ZB)
   10 CONTINUE
C-----------------------------------------------------------------------
C     K(FNU,Z) IS COMPUTED FROM H(2,FNU,-I*Z) WHERE Z IS IN THE FIRST
C     QUADRANT. FOURTH QUADRANT VALUES (YY.LE.0.0E0) ARE COMPUTED BY
C     CONJUGATION SINCE THE K FUNCTION IS REAL ON THE POSITIVE REAL AXIS
C-----------------------------------------------------------------------
      J = 2
      DO 70 I=1,N
C-----------------------------------------------------------------------
C     J FLIP FLOPS BETWEEN 1 AND 2 IN J = 3 - J
C-----------------------------------------------------------------------
        J = 3 - J
        FN = FNU + FLOAT(I-1)
        CALL CUNHJ(ZN, FN, 0, TOL, PHI(J), ARG(J), ZETA1(J), ZETA2(J),
     *   ASUM(J), BSUM(J))
        IF (KODE.EQ.1) GO TO 20
        CFN = CMPLX(FN,0.0E0)
        S1 = ZETA1(J) - CFN*(CFN/(ZB+ZETA2(J)))
        GO TO 30
   20   CONTINUE
        S1 = ZETA1(J) - ZETA2(J)
   30   CONTINUE
C-----------------------------------------------------------------------
C     TEST FOR UNDERFLOW AND OVERFLOW
C-----------------------------------------------------------------------
        RS1 = REAL(S1)
        IF (ABS(RS1).GT.ELIM) GO TO 60
        IF (KDFLG.EQ.1) KFLAG = 2
        IF (ABS(RS1).LT.ALIM) GO TO 40
C-----------------------------------------------------------------------
C     REFINE  TEST AND SCALE
C-----------------------------------------------------------------------
        APHI = CABS(PHI(J))
        AARG = CABS(ARG(J))
        RS1 = RS1 + ALOG(APHI) - 0.25E0*ALOG(AARG) - AIC
        IF (ABS(RS1).GT.ELIM) GO TO 60
        IF (KDFLG.EQ.1) KFLAG = 1
        IF (RS1.LT.0.0E0) GO TO 40
        IF (KDFLG.EQ.1) KFLAG = 3
   40   CONTINUE
C-----------------------------------------------------------------------
C     SCALE S1 TO KEEP INTERMEDIATE ARITHMETIC ON SCALE NEAR
C     EXPONENT EXTREMES
C-----------------------------------------------------------------------
        C2 = ARG(J)*CR2
        CALL CAIRY(C2, 0, 2, AI, NAI, IDUM)
        CALL CAIRY(C2, 1, 2, DAI, NDAI, IDUM)
        S2 = CS*PHI(J)*(AI*ASUM(J)+CR2*DAI*BSUM(J))
        C2R = REAL(S1)
        C2I = AIMAG(S1)
        C2M = EXP(C2R)*REAL(CSS(KFLAG))
        S1 = CMPLX(C2M,0.0E0)*CMPLX(COS(C2I),SIN(C2I))
        S2 = S2*S1
        IF (KFLAG.NE.1) GO TO 50
        CALL CUCHK(S2, NW, BRY(1), TOL)
        IF (NW.NE.0) GO TO 60
   50   CONTINUE
        IF (YY.LE.0.0E0) S2 = CONJG(S2)
        CY(KDFLG) = S2
        Y(I) = S2*CSR(KFLAG)
        CS = -CI*CS
        IF (KDFLG.EQ.2) GO TO 75
        KDFLG = 2
        GO TO 70
   60   CONTINUE
        IF (RS1.GT.0.0E0) GO TO 300
C-----------------------------------------------------------------------
C     FOR X.LT.0.0, THE I FUNCTION TO BE ADDED WILL OVERFLOW
C-----------------------------------------------------------------------
        IF (X.LT.0.0E0) GO TO 300
        KDFLG = 1
        Y(I) = CZERO
        CS = -CI*CS
        NZ=NZ+1
        IF (I.EQ.1) GO TO 70
        IF (Y(I-1).EQ.CZERO) GO TO 70
        Y(I-1) = CZERO
        NZ=NZ+1
   70 CONTINUE
      I=N
   75 CONTINUE
      RZ = CMPLX(2.0E0,0.0E0)/ZR
      CK = CMPLX(FN,0.0E0)*RZ
      IB = I + 1
      IF (N.LT.IB) GO TO 170
C-----------------------------------------------------------------------
C     TEST LAST MEMBER FOR UNDERFLOW AND OVERFLOW, SET SEQUENCE TO ZERO
C     ON UNDERFLOW
C-----------------------------------------------------------------------
      FN = FNU+FLOAT(N-1)
      IPARD = 1
      IF (MR.NE.0) IPARD = 0
      CALL CUNHJ(ZN,FN,IPARD,TOL,PHID,ARGD,ZETA1D,ZETA2D,ASUMD,BSUMD)
      IF (KODE.EQ.1) GO TO 80
      CFN=CMPLX(FN,0.0E0)
      S1=ZETA1D-CFN*(CFN/(ZB+ZETA2D))
      GO TO 90
   80 CONTINUE
      S1=ZETA1D-ZETA2D
   90 CONTINUE
      RS1=REAL(S1)
      IF (ABS(RS1).GT.ELIM) GO TO 95
      IF (ABS(RS1).LT.ALIM) GO TO 100
C-----------------------------------------------------------------------
C     REFINE ESTIMATE AND TEST
C-----------------------------------------------------------------------
      APHI=CABS(PHID)
      AARG = CABS(ARGD)
      RS1=RS1+ALOG(APHI)-0.25E0*ALOG(AARG)-AIC
      IF (ABS(RS1).LT.ELIM) GO TO 100
   95 CONTINUE
      IF (RS1.GT.0.0E0) GO TO 300
C-----------------------------------------------------------------------
C     FOR X.LT.0.0, THE I FUNCTION TO BE ADDED WILL OVERFLOW
C-----------------------------------------------------------------------
      IF (X.LT.0.0E0) GO TO 300
      NZ=N
      DO 96 I=1,N
        Y(I) = CZERO
   96 CONTINUE
      RETURN
  100 CONTINUE
C-----------------------------------------------------------------------
C     SCALED FORWARD RECURRENCE FOR REMAINDER OF THE SEQUENCE
C-----------------------------------------------------------------------
      S1 = CY(1)
      S2 = CY(2)
      C1 = CSR(KFLAG)
      ASCLE = BRY(KFLAG)
      DO 120 I=IB,N
        C2 = S2
        S2 = CK*S2 + S1
        S1 = C2
        CK = CK + RZ
        C2 = S2*C1
        Y(I) = C2
        IF (KFLAG.GE.3) GO TO 120
        C2R = REAL(C2)
        C2I = AIMAG(C2)
        C2R = ABS(C2R)
        C2I = ABS(C2I)
        C2M = AMAX1(C2R,C2I)
        IF (C2M.LE.ASCLE) GO TO 120
        KFLAG = KFLAG + 1
        ASCLE = BRY(KFLAG)
        S1 = S1*C1
        S2 = C2
        S1 = S1*CSS(KFLAG)
        S2 = S2*CSS(KFLAG)
        C1 = CSR(KFLAG)
  120 CONTINUE
  170 CONTINUE
      IF (MR.EQ.0) RETURN
C-----------------------------------------------------------------------
C     ANALYTIC CONTINUATION FOR RE(Z).LT.0.0E0
C-----------------------------------------------------------------------
      NZ = 0
      FMR = FLOAT(MR)
      SGN = -SIGN(PI,FMR)
C-----------------------------------------------------------------------
C     CSPN AND CSGN ARE COEFF OF K AND I FUNCTIONS RESP.
C-----------------------------------------------------------------------
      CSGN = CMPLX(0.0E0,SGN)
      IF (YY.LE.0.0E0) CSGN = CONJG(CSGN)
      IFN = INU + N - 1
      ANG = FNF*SGN
      CPN = COS(ANG)
      SPN = SIN(ANG)
      CSPN = CMPLX(CPN,SPN)
      IF (MOD(IFN,2).EQ.1) CSPN = -CSPN
C-----------------------------------------------------------------------
C     CS=COEFF OF THE J FUNCTION TO GET THE I FUNCTION. I(FNU,Z) IS
C     COMPUTED FROM EXP(I*FNU*HPI)*J(FNU,-I*Z) WHERE Z IS IN THE FIRST
C     QUADRANT. FOURTH QUADRANT VALUES (YY.LE.0.0E0) ARE COMPUTED BY
C     CONJUGATION SINCE THE I FUNCTION IS REAL ON THE POSITIVE REAL AXIS
C-----------------------------------------------------------------------
      CS = CMPLX(CAR,-SAR)*CSGN
      IN = MOD(IFN,4) + 1
      C2 = CIP(IN)
      CS = CS*CONJG(C2)
      ASC = BRY(1)
      KK = N
      KDFLG = 1
      IB = IB-1
      IC = IB-1
      IUF = 0
      DO 270 K=1,N
C-----------------------------------------------------------------------
C     LOGIC TO SORT OUT CASES WHOSE PARAMETERS WERE SET FOR THE K
C     FUNCTION ABOVE
C-----------------------------------------------------------------------
        FN = FNU+FLOAT(KK-1)
        IF (N.GT.2) GO TO 180
  175   CONTINUE
        PHID = PHI(J)
        ARGD = ARG(J)
        ZETA1D = ZETA1(J)
        ZETA2D = ZETA2(J)
        ASUMD = ASUM(J)
        BSUMD = BSUM(J)
        J = 3 - J
        GO TO 190
  180   CONTINUE
        IF ((KK.EQ.N).AND.(IB.LT.N)) GO TO 190
        IF ((KK.EQ.IB).OR.(KK.EQ.IC)) GO TO 175
        CALL CUNHJ(ZN, FN, 0, TOL, PHID, ARGD, ZETA1D, ZETA2D,
     *   ASUMD, BSUMD)
  190   CONTINUE
        IF (KODE.EQ.1) GO TO 200
        CFN = CMPLX(FN,0.0E0)
        S1 = -ZETA1D + CFN*(CFN/(ZB+ZETA2D))
        GO TO 210
  200   CONTINUE
        S1 = -ZETA1D + ZETA2D
  210   CONTINUE
C-----------------------------------------------------------------------
C     TEST FOR UNDERFLOW AND OVERFLOW
C-----------------------------------------------------------------------
        RS1 = REAL(S1)
        IF (ABS(RS1).GT.ELIM) GO TO 260
        IF (KDFLG.EQ.1) IFLAG = 2
        IF (ABS(RS1).LT.ALIM) GO TO 220
C-----------------------------------------------------------------------
C     REFINE  TEST AND SCALE
C-----------------------------------------------------------------------
        APHI = CABS(PHID)
        AARG = CABS(ARGD)
        RS1 = RS1 + ALOG(APHI) - 0.25E0*ALOG(AARG) - AIC
        IF (ABS(RS1).GT.ELIM) GO TO 260
        IF (KDFLG.EQ.1) IFLAG = 1
        IF (RS1.LT.0.0E0) GO TO 220
        IF (KDFLG.EQ.1) IFLAG = 3
  220   CONTINUE
        CALL CAIRY(ARGD, 0, 2, AI, NAI, IDUM)
        CALL CAIRY(ARGD, 1, 2, DAI, NDAI, IDUM)
        S2 = CS*PHID*(AI*ASUMD+DAI*BSUMD)
        C2R = REAL(S1)
        C2I = AIMAG(S1)
        C2M = EXP(C2R)*REAL(CSS(IFLAG))
        S1 = CMPLX(C2M,0.0E0)*CMPLX(COS(C2I),SIN(C2I))
        S2 = S2*S1
        IF (IFLAG.NE.1) GO TO 230
        CALL CUCHK(S2, NW, BRY(1), TOL)
        IF (NW.NE.0) S2 = CMPLX(0.0E0,0.0E0)
  230   CONTINUE
        IF (YY.LE.0.0E0) S2 = CONJG(S2)
        CY(KDFLG) = S2
        C2 = S2
        S2 = S2*CSR(IFLAG)
C-----------------------------------------------------------------------
C     ADD I AND K FUNCTIONS, K SEQUENCE IN Y(I), I=1,N
C-----------------------------------------------------------------------
        S1 = Y(KK)
        IF (KODE.EQ.1) GO TO 250
        CALL CS1S2(ZR, S1, S2, NW, ASC, ALIM, IUF)
        NZ = NZ + NW
  250   CONTINUE
        Y(KK) = S1*CSPN + S2
        KK = KK - 1
        CSPN = -CSPN
        CS = -CS*CI
        IF (C2.NE.CZERO) GO TO 255
        KDFLG = 1
        GO TO 270
  255   CONTINUE
        IF (KDFLG.EQ.2) GO TO 275
        KDFLG = 2
        GO TO 270
  260   CONTINUE
        IF (RS1.GT.0.0E0) GO TO 300
        S2 = CZERO
        GO TO 230
  270 CONTINUE
      K = N
  275 CONTINUE
      IL = N-K
      IF (IL.EQ.0) RETURN
C-----------------------------------------------------------------------
C     RECUR BACKWARD FOR REMAINDER OF I SEQUENCE AND ADD IN THE
C     K FUNCTIONS, SCALING THE I SEQUENCE DURING RECURRENCE TO KEEP
C     INTERMEDIATE ARITHMETIC ON SCALE NEAR EXPONENT EXTREMES.
C-----------------------------------------------------------------------
      S1 = CY(1)
      S2 = CY(2)
      CS = CSR(IFLAG)
      ASCLE = BRY(IFLAG)
      FN = FLOAT(INU+IL)
      DO 290 I=1,IL
        C2 = S2
        S2 = S1 + CMPLX(FN+FNF,0.0E0)*RZ*S2
        S1 = C2
        FN = FN - 1.0E0
        C2 = S2*CS
        CK = C2
        C1 = Y(KK)
        IF (KODE.EQ.1) GO TO 280
        CALL CS1S2(ZR, C1, C2, NW, ASC, ALIM, IUF)
        NZ = NZ + NW
  280   CONTINUE
        Y(KK) = C1*CSPN + C2
        KK = KK - 1
        CSPN = -CSPN
        IF (IFLAG.GE.3) GO TO 290
        C2R = REAL(CK)
        C2I = AIMAG(CK)
        C2R = ABS(C2R)
        C2I = ABS(C2I)
        C2M = AMAX1(C2R,C2I)
        IF (C2M.LE.ASCLE) GO TO 290
        IFLAG = IFLAG + 1
        ASCLE = BRY(IFLAG)
        S1 = S1*CS
        S2 = CK
        S1 = S1*CSS(IFLAG)
        S2 = S2*CSS(IFLAG)
        CS = CSR(IFLAG)
  290 CONTINUE
      RETURN
  300 CONTINUE
      NZ = -1
      RETURN
      END

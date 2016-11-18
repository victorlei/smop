      SUBROUTINE CUNK1(Z, FNU, KODE, MR, N, Y, NZ, TOL, ELIM, ALIM)
C***BEGIN PROLOGUE  CUNK1
C***REFER TO  CBESK
C
C     CUNK1 COMPUTES K(FNU,Z) AND ITS ANALYTIC CONTINUATION FROM THE
C     RIGHT HALF PLANE TO THE LEFT HALF PLANE BY MEANS OF THE
C     UNIFORM ASYMPTOTIC EXPANSION.
C     MR INDICATES THE DIRECTION OF ROTATION FOR ANALYTIC CONTINUATION.
C     NZ=-1 MEANS AN OVERFLOW WILL OCCUR
C
C***ROUTINES CALLED  CS1S2,CUCHK,CUNIK,R1MACH
C***END PROLOGUE  CUNK1
      COMPLEX CFN, CK, CONE, CRSC, CS, CSCL, CSGN, CSPN, CSR, CSS,
     * CWRK, CY, CZERO, C1, C2, PHI,  RZ, SUM,  S1, S2, Y, Z,
     * ZETA1,  ZETA2,  ZR, PHID, ZETA1D, ZETA2D, SUMD
      REAL ALIM, ANG, APHI, ASC, ASCLE, BRY, CPN, C2I, C2M, C2R, ELIM,
     * FMR, FN, FNF, FNU, PI, RS1, SGN, SPN, TOL, X, R1MACH
      INTEGER I, IB, IFLAG, IFN, IL, INIT, INU, IUF, K, KDFLG, KFLAG,
     * KK, KODE, MR, N, NW, NZ, J, IPARD, INITD, IC
      DIMENSION BRY(3), INIT(2), Y(N), SUM(2), PHI(2), ZETA1(2),
     * ZETA2(2), CY(2), CWRK(16,3), CSS(3), CSR(3)
      DATA CZERO, CONE / (0.0E0,0.0E0) , (1.0E0,0.0E0) /
      DATA PI / 3.14159265358979324E0 /
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
      J=2
      DO 70 I=1,N
C-----------------------------------------------------------------------
C     J FLIP FLOPS BETWEEN 1 AND 2 IN J = 3 - J
C-----------------------------------------------------------------------
        J = 3 - J
        FN = FNU + FLOAT(I-1)
        INIT(J) = 0
        CALL CUNIK(ZR, FN, 2, 0, TOL, INIT(J), PHI(J), ZETA1(J),
     *   ZETA2(J), SUM(J), CWRK(1,J))
        IF (KODE.EQ.1) GO TO 20
        CFN = CMPLX(FN,0.0E0)
        S1 = ZETA1(J) - CFN*(CFN/(ZR+ZETA2(J)))
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
        RS1 = RS1 + ALOG(APHI)
        IF (ABS(RS1).GT.ELIM) GO TO 60
        IF (KDFLG.EQ.1) KFLAG = 1
        IF (RS1.LT.0.0E0) GO TO 40
        IF (KDFLG.EQ.1) KFLAG = 3
   40   CONTINUE
C-----------------------------------------------------------------------
C     SCALE S1 TO KEEP INTERMEDIATE ARITHMETIC ON SCALE NEAR
C     EXPONENT EXTREMES
C-----------------------------------------------------------------------
        S2 = PHI(J)*SUM(J)
        C2R = REAL(S1)
        C2I = AIMAG(S1)
        C2M = EXP(C2R)*REAL(CSS(KFLAG))
        S1 = CMPLX(C2M,0.0E0)*CMPLX(COS(C2I),SIN(C2I))
        S2 = S2*S1
        IF (KFLAG.NE.1) GO TO 50
        CALL CUCHK(S2, NW, BRY(1), TOL)
        IF (NW.NE.0) GO TO 60
   50   CONTINUE
        CY(KDFLG) = S2
        Y(I) = S2*CSR(KFLAG)
        IF (KDFLG.EQ.2) GO TO 75
        KDFLG = 2
        GO TO 70
   60   CONTINUE
        IF (RS1.GT.0.0E0) GO TO 290
C-----------------------------------------------------------------------
C     FOR X.LT.0.0, THE I FUNCTION TO BE ADDED WILL OVERFLOW
C-----------------------------------------------------------------------
        IF (X.LT.0.0E0) GO TO 290
        KDFLG = 1
        Y(I) = CZERO
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
      IB = I+1
      IF (N.LT.IB) GO TO 160
C-----------------------------------------------------------------------
C     TEST LAST MEMBER FOR UNDERFLOW AND OVERFLOW, SET SEQUENCE TO ZERO
C     ON UNDERFLOW
C-----------------------------------------------------------------------
      FN = FNU+FLOAT(N-1)
      IPARD = 1
      IF (MR.NE.0) IPARD = 0
      INITD = 0
      CALL CUNIK(ZR,FN,2,IPARD,TOL,INITD,PHID,ZETA1D,ZETA2D,SUMD,
     *CWRK(1,3))
      IF (KODE.EQ.1) GO TO 80
      CFN=CMPLX(FN,0.0E0)
      S1=ZETA1D-CFN*(CFN/(ZR+ZETA2D))
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
      RS1=RS1+ALOG(APHI)
      IF (ABS(RS1).LT.ELIM) GO TO 100
   95 CONTINUE
      IF (RS1.GT.0.0E0) GO TO 290
C-----------------------------------------------------------------------
C     FOR X.LT.0.0, THE I FUNCTION TO BE ADDED WILL OVERFLOW
C-----------------------------------------------------------------------
      IF (X.LT.0.0E0) GO TO 290
      NZ=N
      DO 96 I=1,N
        Y(I) = CZERO
   96 CONTINUE
      RETURN
  100 CONTINUE
C-----------------------------------------------------------------------
C     RECUR FORWARD FOR REMAINDER OF THE SEQUENCE
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
  160 CONTINUE
      IF (MR.EQ.0) RETURN
C-----------------------------------------------------------------------
C     ANALYTIC CONTINUATION FOR RE(Z).LT.0.0E0
C-----------------------------------------------------------------------
      NZ = 0
      FMR = FLOAT(MR)
      SGN = -SIGN(PI,FMR)
C-----------------------------------------------------------------------
C     CSPN AND CSGN ARE COEFF OF K AND I FUNCIONS RESP.
C-----------------------------------------------------------------------
      CSGN = CMPLX(0.0E0,SGN)
      INU = INT(FNU)
      FNF = FNU - FLOAT(INU)
      IFN = INU + N - 1
      ANG = FNF*SGN
      CPN = COS(ANG)
      SPN = SIN(ANG)
      CSPN = CMPLX(CPN,SPN)
      IF (MOD(IFN,2).EQ.1) CSPN = -CSPN
      ASC = BRY(1)
      KK = N
      IUF = 0
      KDFLG = 1
      IB = IB-1
      IC = IB-1
      DO 260 K=1,N
        FN = FNU + FLOAT(KK-1)
C-----------------------------------------------------------------------
C     LOGIC TO SORT OUT CASES WHOSE PARAMETERS WERE SET FOR THE K
C     FUNCTION ABOVE
C-----------------------------------------------------------------------
        M=3
        IF (N.GT.2) GO TO 175
  170   CONTINUE
        INITD = INIT(J)
        PHID = PHI(J)
        ZETA1D = ZETA1(J)
        ZETA2D = ZETA2(J)
        SUMD = SUM(J)
        M = J
        J = 3 - J
        GO TO 180
  175   CONTINUE
        IF ((KK.EQ.N).AND.(IB.LT.N)) GO TO 180
        IF ((KK.EQ.IB).OR.(KK.EQ.IC)) GO TO 170
        INITD = 0
  180   CONTINUE
        CALL CUNIK(ZR, FN, 1, 0, TOL, INITD, PHID, ZETA1D,
     *   ZETA2D, SUMD, CWRK(1,M))
        IF (KODE.EQ.1) GO TO 190
        CFN = CMPLX(FN,0.0E0)
        S1 = -ZETA1D + CFN*(CFN/(ZR+ZETA2D))
        GO TO 200
  190   CONTINUE
        S1 = -ZETA1D + ZETA2D
  200   CONTINUE
C-----------------------------------------------------------------------
C     TEST FOR UNDERFLOW AND OVERFLOW
C-----------------------------------------------------------------------
        RS1 = REAL(S1)
        IF (ABS(RS1).GT.ELIM) GO TO 250
        IF (KDFLG.EQ.1) IFLAG = 2
        IF (ABS(RS1).LT.ALIM) GO TO 210
C-----------------------------------------------------------------------
C     REFINE  TEST AND SCALE
C-----------------------------------------------------------------------
        APHI = CABS(PHID)
        RS1 = RS1 + ALOG(APHI)
        IF (ABS(RS1).GT.ELIM) GO TO 250
        IF (KDFLG.EQ.1) IFLAG = 1
        IF (RS1.LT.0.0E0) GO TO 210
        IF (KDFLG.EQ.1) IFLAG = 3
  210   CONTINUE
        S2 = CSGN*PHID*SUMD
        C2R = REAL(S1)
        C2I = AIMAG(S1)
        C2M = EXP(C2R)*REAL(CSS(IFLAG))
        S1 = CMPLX(C2M,0.0E0)*CMPLX(COS(C2I),SIN(C2I))
        S2 = S2*S1
        IF (IFLAG.NE.1) GO TO 220
        CALL CUCHK(S2, NW, BRY(1), TOL)
        IF (NW.NE.0) S2 = CMPLX(0.0E0,0.0E0)
  220   CONTINUE
        CY(KDFLG) = S2
        C2 = S2
        S2 = S2*CSR(IFLAG)
C-----------------------------------------------------------------------
C     ADD I AND K FUNCTIONS, K SEQUENCE IN Y(I), I=1,N
C-----------------------------------------------------------------------
        S1 = Y(KK)
        IF (KODE.EQ.1) GO TO 240
        CALL CS1S2(ZR, S1, S2, NW, ASC, ALIM, IUF)
        NZ = NZ + NW
  240   CONTINUE
        Y(KK) = S1*CSPN + S2
        KK = KK - 1
        CSPN = -CSPN
        IF (C2.NE.CZERO) GO TO 245
        KDFLG = 1
        GO TO 260
  245   CONTINUE
        IF (KDFLG.EQ.2) GO TO 265
        KDFLG = 2
        GO TO 260
  250   CONTINUE
        IF (RS1.GT.0.0E0) GO TO 290
        S2 = CZERO
        GO TO 220
  260 CONTINUE
      K = N
  265 CONTINUE
      IL = N - K
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
      DO 280 I=1,IL
        C2 = S2
        S2 = S1 + CMPLX(FN+FNF,0.0E0)*RZ*S2
        S1 = C2
        FN = FN - 1.0E0
        C2 = S2*CS
        CK = C2
        C1 = Y(KK)
        IF (KODE.EQ.1) GO TO 270
        CALL CS1S2(ZR, C1, C2, NW, ASC, ALIM, IUF)
        NZ = NZ + NW
  270   CONTINUE
        Y(KK) = C1*CSPN + C2
        KK = KK - 1
        CSPN = -CSPN
        IF (IFLAG.GE.3) GO TO 280
        C2R = REAL(CK)
        C2I = AIMAG(CK)
        C2R = ABS(C2R)
        C2I = ABS(C2I)
        C2M = AMAX1(C2R,C2I)
        IF (C2M.LE.ASCLE) GO TO 280
        IFLAG = IFLAG + 1
        ASCLE = BRY(IFLAG)
        S1 = S1*CS
        S2 = CK
        S1 = S1*CSS(IFLAG)
        S2 = S2*CSS(IFLAG)
        CS = CSR(IFLAG)
  280 CONTINUE
      RETURN
  290 CONTINUE
      NZ = -1
      RETURN
      END

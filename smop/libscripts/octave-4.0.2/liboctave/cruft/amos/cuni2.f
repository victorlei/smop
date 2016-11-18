      SUBROUTINE CUNI2(Z, FNU, KODE, N, Y, NZ, NLAST, FNUL, TOL, ELIM,
     * ALIM)
C***BEGIN PROLOGUE  CUNI2
C***REFER TO  CBESI,CBESK
C
C     CUNI2 COMPUTES I(FNU,Z) IN THE RIGHT HALF PLANE BY MEANS OF
C     UNIFORM ASYMPTOTIC EXPANSION FOR J(FNU,ZN) WHERE ZN IS Z*I
C     OR -Z*I AND ZN IS IN THE RIGHT HALF PLANE ALSO.
C
C     FNUL IS THE SMALLEST ORDER PERMITTED FOR THE ASYMPTOTIC
C     EXPANSION. NLAST=0 MEANS ALL OF THE Y VALUES WERE SET.
C     NLAST.NE.0 IS THE NUMBER LEFT TO BE COMPUTED BY ANOTHER
C     FORMULA FOR ORDERS FNU TO FNU+NLAST-1 BECAUSE FNU+NLAST-1.LT.FNUL.
C     Y(I)=CZERO FOR I=NLAST+1,N
C
C***ROUTINES CALLED  CAIRY,CUCHK,CUNHJ,CUOIK,R1MACH
C***END PROLOGUE  CUNI2
      COMPLEX AI, ARG, ASUM, BSUM, CFN, CI, CID, CIP, CONE, CRSC, CSCL,
     * CSR, CSS, CY, CZERO, C1, C2, DAI, PHI, RZ, S1, S2, Y, Z, ZB,
     * ZETA1, ZETA2, ZN, ZAR
      REAL AARG, AIC, ALIM, ANG, APHI, ASCLE, AY, BRY, CAR, C2I, C2M,
     * C2R, ELIM, FN, FNU, FNUL, HPI, RS1, SAR, TOL, YY, R1MACH
      INTEGER I, IFLAG, IN, INU, J, K, KODE, N, NAI, ND, NDAI, NLAST,
     * NN, NUF, NW, NZ, IDUM
      DIMENSION BRY(3), Y(N), CIP(4), CSS(3), CSR(3), CY(2)
      DATA CZERO,CONE,CI/(0.0E0,0.0E0),(1.0E0,0.0E0),(0.0E0,1.0E0)/
      DATA CIP(1),CIP(2),CIP(3),CIP(4)/
     1 (1.0E0,0.0E0), (0.0E0,1.0E0), (-1.0E0,0.0E0), (0.0E0,-1.0E0)/
      DATA HPI, AIC  /
     1      1.57079632679489662E+00,     1.265512123484645396E+00/
C
      NZ = 0
      ND = N
      NLAST = 0
C-----------------------------------------------------------------------
C     COMPUTED VALUES WITH EXPONENTS BETWEEN ALIM AND ELIM IN MAG-
C     NITUDE ARE SCALED TO KEEP INTERMEDIATE ARITHMETIC ON SCALE,
C     EXP(ALIM)=EXP(ELIM)*TOL
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
      YY = AIMAG(Z)
C-----------------------------------------------------------------------
C     ZN IS IN THE RIGHT HALF PLANE AFTER ROTATION BY CI OR -CI
C-----------------------------------------------------------------------
      ZN = -Z*CI
      ZB = Z
      CID = -CI
      INU = INT(FNU)
      ANG = HPI*(FNU-FLOAT(INU))
      CAR = COS(ANG)
      SAR = SIN(ANG)
      C2 = CMPLX(CAR,SAR)
      ZAR = C2
      IN = INU + N - 1
      IN = MOD(IN,4)
      C2 = C2*CIP(IN+1)
      IF (YY.GT.0.0E0) GO TO 10
      ZN = CONJG(-ZN)
      ZB = CONJG(ZB)
      CID = -CID
      C2 = CONJG(C2)
   10 CONTINUE
C-----------------------------------------------------------------------
C     CHECK FOR UNDERFLOW AND OVERFLOW ON FIRST MEMBER
C-----------------------------------------------------------------------
      FN = AMAX1(FNU,1.0E0)
      CALL CUNHJ(ZN, FN, 1, TOL, PHI, ARG, ZETA1, ZETA2, ASUM, BSUM)
      IF (KODE.EQ.1) GO TO 20
      CFN = CMPLX(FNU,0.0E0)
      S1 = -ZETA1 + CFN*(CFN/(ZB+ZETA2))
      GO TO 30
   20 CONTINUE
      S1 = -ZETA1 + ZETA2
   30 CONTINUE
      RS1 = REAL(S1)
      IF (ABS(RS1).GT.ELIM) GO TO 150
   40 CONTINUE
      NN = MIN0(2,ND)
      DO 90 I=1,NN
        FN = FNU + FLOAT(ND-I)
        CALL CUNHJ(ZN, FN, 0, TOL, PHI, ARG, ZETA1, ZETA2, ASUM, BSUM)
        IF (KODE.EQ.1) GO TO 50
        CFN = CMPLX(FN,0.0E0)
        AY = ABS(YY)
        S1 = -ZETA1 + CFN*(CFN/(ZB+ZETA2)) + CMPLX(0.0E0,AY)
        GO TO 60
   50   CONTINUE
        S1 = -ZETA1 + ZETA2
   60   CONTINUE
C-----------------------------------------------------------------------
C     TEST FOR UNDERFLOW AND OVERFLOW
C-----------------------------------------------------------------------
        RS1 = REAL(S1)
        IF (ABS(RS1).GT.ELIM) GO TO 120
        IF (I.EQ.1) IFLAG = 2
        IF (ABS(RS1).LT.ALIM) GO TO 70
C-----------------------------------------------------------------------
C     REFINE  TEST AND SCALE
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
        APHI = CABS(PHI)
        AARG = CABS(ARG)
        RS1 = RS1 + ALOG(APHI) - 0.25E0*ALOG(AARG) - AIC
        IF (ABS(RS1).GT.ELIM) GO TO 120
        IF (I.EQ.1) IFLAG = 1
        IF (RS1.LT.0.0E0) GO TO 70
        IF (I.EQ.1) IFLAG = 3
   70   CONTINUE
C-----------------------------------------------------------------------
C     SCALE S1 TO KEEP INTERMEDIATE ARITHMETIC ON SCALE NEAR
C     EXPONENT EXTREMES
C-----------------------------------------------------------------------
        CALL CAIRY(ARG, 0, 2, AI, NAI, IDUM)
        CALL CAIRY(ARG, 1, 2, DAI, NDAI, IDUM)
        S2 = PHI*(AI*ASUM+DAI*BSUM)
        C2R = REAL(S1)
        C2I = AIMAG(S1)
        C2M = EXP(C2R)*REAL(CSS(IFLAG))
        S1 = CMPLX(C2M,0.0E0)*CMPLX(COS(C2I),SIN(C2I))
        S2 = S2*S1
        IF (IFLAG.NE.1) GO TO 80
        CALL CUCHK(S2, NW, BRY(1), TOL)
        IF (NW.NE.0) GO TO 120
   80   CONTINUE
        IF (YY.LE.0.0E0) S2 = CONJG(S2)
        J = ND - I + 1
        S2 = S2*C2
        CY(I) = S2
        Y(J) = S2*CSR(IFLAG)
        C2 = C2*CID
   90 CONTINUE
      IF (ND.LE.2) GO TO 110
      RZ = CMPLX(2.0E0,0.0E0)/Z
      BRY(2) = 1.0E0/BRY(1)
      BRY(3) = R1MACH(2)
      S1 = CY(1)
      S2 = CY(2)
      C1 = CSR(IFLAG)
      ASCLE = BRY(IFLAG)
      K = ND - 2
      FN = FLOAT(K)
      DO 100 I=3,ND
        C2 = S2
        S2 = S1 + CMPLX(FNU+FN,0.0E0)*RZ*S2
        S1 = C2
        C2 = S2*C1
        Y(K) = C2
        K = K - 1
        FN = FN - 1.0E0
        IF (IFLAG.GE.3) GO TO 100
        C2R = REAL(C2)
        C2I = AIMAG(C2)
        C2R = ABS(C2R)
        C2I = ABS(C2I)
        C2M = AMAX1(C2R,C2I)
        IF (C2M.LE.ASCLE) GO TO 100
        IFLAG = IFLAG + 1
        ASCLE = BRY(IFLAG)
        S1 = S1*C1
        S2 = C2
        S1 = S1*CSS(IFLAG)
        S2 = S2*CSS(IFLAG)
        C1 = CSR(IFLAG)
  100 CONTINUE
  110 CONTINUE
      RETURN
  120 CONTINUE
      IF (RS1.GT.0.0E0) GO TO 140
C-----------------------------------------------------------------------
C     SET UNDERFLOW AND UPDATE PARAMETERS
C-----------------------------------------------------------------------
      Y(ND) = CZERO
      NZ = NZ + 1
      ND = ND - 1
      IF (ND.EQ.0) GO TO 110
      CALL CUOIK(Z, FNU, KODE, 1, ND, Y, NUF, TOL, ELIM, ALIM)
      IF (NUF.LT.0) GO TO 140
      ND = ND - NUF
      NZ = NZ + NUF
      IF (ND.EQ.0) GO TO 110
      FN = FNU + FLOAT(ND-1)
      IF (FN.LT.FNUL) GO TO 130
C      FN = AIMAG(CID)
C      J = NUF + 1
C      K = MOD(J,4) + 1
C      S1 = CIP(K)
C      IF (FN.LT.0.0E0) S1 = CONJG(S1)
C      C2 = C2*S1
      IN = INU + ND - 1
      IN = MOD(IN,4) + 1
      C2 = ZAR*CIP(IN)
      IF (YY.LE.0.0E0)C2=CONJG(C2)
      GO TO 40
  130 CONTINUE
      NLAST = ND
      RETURN
  140 CONTINUE
      NZ = -1
      RETURN
  150 CONTINUE
      IF (RS1.GT.0.0E0) GO TO 140
      NZ = N
      DO 160 I=1,N
        Y(I) = CZERO
  160 CONTINUE
      RETURN
      END

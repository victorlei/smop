      SUBROUTINE CUNI1(Z, FNU, KODE, N, Y, NZ, NLAST, FNUL, TOL, ELIM,
     * ALIM)
C***BEGIN PROLOGUE  CUNI1
C***REFER TO  CBESI,CBESK
C
C     CUNI1 COMPUTES I(FNU,Z)  BY MEANS OF THE UNIFORM ASYMPTOTIC
C     EXPANSION FOR I(FNU,Z) IN -PI/3.LE.ARG Z.LE.PI/3.
C
C     FNUL IS THE SMALLEST ORDER PERMITTED FOR THE ASYMPTOTIC
C     EXPANSION. NLAST=0 MEANS ALL OF THE Y VALUES WERE SET.
C     NLAST.NE.0 IS THE NUMBER LEFT TO BE COMPUTED BY ANOTHER
C     FORMULA FOR ORDERS FNU TO FNU+NLAST-1 BECAUSE FNU+NLAST-1.LT.FNUL.
C     Y(I)=CZERO FOR I=NLAST+1,N
C
C***ROUTINES CALLED  CUCHK,CUNIK,CUOIK,R1MACH
C***END PROLOGUE  CUNI1
      COMPLEX CFN, CONE, CRSC, CSCL, CSR, CSS, CWRK, CZERO, C1, C2,
     * PHI, RZ, SUM, S1, S2, Y, Z, ZETA1, ZETA2, CY
      REAL ALIM, APHI, ASCLE, BRY, C2I, C2M, C2R, ELIM, FN, FNU, FNUL,
     * RS1, TOL, YY, R1MACH
      INTEGER I, IFLAG, INIT, K, KODE, M, N, ND, NLAST, NN, NUF, NW, NZ
      DIMENSION BRY(3), Y(N), CWRK(16), CSS(3), CSR(3), CY(2)
      DATA CZERO, CONE / (0.0E0,0.0E0), (1.0E0,0.0E0) /
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
C-----------------------------------------------------------------------
C     CHECK FOR UNDERFLOW AND OVERFLOW ON FIRST MEMBER
C-----------------------------------------------------------------------
      FN = AMAX1(FNU,1.0E0)
      INIT = 0
      CALL CUNIK(Z, FN, 1, 1, TOL, INIT, PHI, ZETA1, ZETA2, SUM, CWRK)
      IF (KODE.EQ.1) GO TO 10
      CFN = CMPLX(FN,0.0E0)
      S1 = -ZETA1 + CFN*(CFN/(Z+ZETA2))
      GO TO 20
   10 CONTINUE
      S1 = -ZETA1 + ZETA2
   20 CONTINUE
      RS1 = REAL(S1)
      IF (ABS(RS1).GT.ELIM) GO TO 130
   30 CONTINUE
      NN = MIN0(2,ND)
      DO 80 I=1,NN
        FN = FNU + FLOAT(ND-I)
        INIT = 0
        CALL CUNIK(Z, FN, 1, 0, TOL, INIT, PHI, ZETA1, ZETA2, SUM, CWRK)
        IF (KODE.EQ.1) GO TO 40
        CFN = CMPLX(FN,0.0E0)
        YY = AIMAG(Z)
        S1 = -ZETA1 + CFN*(CFN/(Z+ZETA2)) + CMPLX(0.0E0,YY)
        GO TO 50
   40   CONTINUE
        S1 = -ZETA1 + ZETA2
   50   CONTINUE
C-----------------------------------------------------------------------
C     TEST FOR UNDERFLOW AND OVERFLOW
C-----------------------------------------------------------------------
        RS1 = REAL(S1)
        IF (ABS(RS1).GT.ELIM) GO TO 110
        IF (I.EQ.1) IFLAG = 2
        IF (ABS(RS1).LT.ALIM) GO TO 60
C-----------------------------------------------------------------------
C     REFINE  TEST AND SCALE
C-----------------------------------------------------------------------
        APHI = CABS(PHI)
        RS1 = RS1 + ALOG(APHI)
        IF (ABS(RS1).GT.ELIM) GO TO 110
        IF (I.EQ.1) IFLAG = 1
        IF (RS1.LT.0.0E0) GO TO 60
        IF (I.EQ.1) IFLAG = 3
   60   CONTINUE
C-----------------------------------------------------------------------
C     SCALE S1 IF CABS(S1).LT.ASCLE
C-----------------------------------------------------------------------
        S2 = PHI*SUM
        C2R = REAL(S1)
        C2I = AIMAG(S1)
        C2M = EXP(C2R)*REAL(CSS(IFLAG))
        S1 = CMPLX(C2M,0.0E0)*CMPLX(COS(C2I),SIN(C2I))
        S2 = S2*S1
        IF (IFLAG.NE.1) GO TO 70
        CALL CUCHK(S2, NW, BRY(1), TOL)
        IF (NW.NE.0) GO TO 110
   70   CONTINUE
        M = ND - I + 1
        CY(I) = S2
        Y(M) = S2*CSR(IFLAG)
   80 CONTINUE
      IF (ND.LE.2) GO TO 100
      RZ = CMPLX(2.0E0,0.0E0)/Z
      BRY(2) = 1.0E0/BRY(1)
      BRY(3) = R1MACH(2)
      S1 = CY(1)
      S2 = CY(2)
      C1 = CSR(IFLAG)
      ASCLE = BRY(IFLAG)
      K = ND - 2
      FN = FLOAT(K)
      DO 90 I=3,ND
        C2 = S2
        S2 = S1 + CMPLX(FNU+FN,0.0E0)*RZ*S2
        S1 = C2
        C2 = S2*C1
        Y(K) = C2
        K = K - 1
        FN = FN - 1.0E0
        IF (IFLAG.GE.3) GO TO 90
        C2R = REAL(C2)
        C2I = AIMAG(C2)
        C2R = ABS(C2R)
        C2I = ABS(C2I)
        C2M = AMAX1(C2R,C2I)
        IF (C2M.LE.ASCLE) GO TO 90
        IFLAG = IFLAG + 1
        ASCLE = BRY(IFLAG)
        S1 = S1*C1
        S2 = C2
        S1 = S1*CSS(IFLAG)
        S2 = S2*CSS(IFLAG)
        C1 = CSR(IFLAG)
   90 CONTINUE
  100 CONTINUE
      RETURN
C-----------------------------------------------------------------------
C     SET UNDERFLOW AND UPDATE PARAMETERS
C-----------------------------------------------------------------------
  110 CONTINUE
      IF (RS1.GT.0.0E0) GO TO 120
      Y(ND) = CZERO
      NZ = NZ + 1
      ND = ND - 1
      IF (ND.EQ.0) GO TO 100
      CALL CUOIK(Z, FNU, KODE, 1, ND, Y, NUF, TOL, ELIM, ALIM)
      IF (NUF.LT.0) GO TO 120
      ND = ND - NUF
      NZ = NZ + NUF
      IF (ND.EQ.0) GO TO 100
      FN = FNU + FLOAT(ND-1)
      IF (FN.GE.FNUL) GO TO 30
      NLAST = ND
      RETURN
  120 CONTINUE
      NZ = -1
      RETURN
  130 CONTINUE
      IF (RS1.GT.0.0E0) GO TO 120
      NZ = N
      DO 140 I=1,N
        Y(I) = CZERO
  140 CONTINUE
      RETURN
      END

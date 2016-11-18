      SUBROUTINE CSERI(Z, FNU, KODE, N, Y, NZ, TOL, ELIM, ALIM)
C***BEGIN PROLOGUE  CSERI
C***REFER TO  CBESI,CBESK
C
C     CSERI COMPUTES THE I BESSEL FUNCTION FOR REAL(Z).GE.0.0 BY
C     MEANS OF THE POWER SERIES FOR LARGE CABS(Z) IN THE
C     REGION CABS(Z).LE.2*SQRT(FNU+1). NZ=0 IS A NORMAL RETURN.
C     NZ.GT.0 MEANS THAT THE LAST NZ COMPONENTS WERE SET TO ZERO
C     DUE TO UNDERFLOW. NZ.LT.0 MEANS UNDERFLOW OCCURRED, BUT THE
C     CONDITION CABS(Z).LE.2*SQRT(FNU+1) WAS VIOLATED AND THE
C     COMPUTATION MUST BE COMPLETED IN ANOTHER ROUTINE WITH N=N-ABS(NZ).
C
C***ROUTINES CALLED  CUCHK,GAMLN,R1MACH
C***END PROLOGUE  CSERI
      COMPLEX AK1, CK, COEF, CONE, CRSC, CZ, CZERO, HZ, RZ, S1, S2, W,
     * Y, Z
      REAL AA, ACZ, AK, ALIM, ARM, ASCLE, ATOL, AZ, DFNU, ELIM, FNU,
     * FNUP, RAK1, RS, RTR1, S, SS, TOL, X, GAMLN, R1MACH
      INTEGER I, IB, IDUM, IFLAG, IL, K, KODE, L, M, N, NN, NW, NZ
      DIMENSION Y(N), W(2)
      DATA CZERO, CONE / (0.0E0,0.0E0), (1.0E0,0.0E0) /
C
      NZ = 0
      AZ = CABS(Z)
      IF (AZ.EQ.0.0E0) GO TO 150
      X = REAL(Z)
      ARM = 1.0E+3*R1MACH(1)
      RTR1 = SQRT(ARM)
      CRSC = CMPLX(1.0E0,0.0E0)
      IFLAG = 0
      IF (AZ.LT.ARM) GO TO 140
      HZ = Z*CMPLX(0.5E0,0.0E0)
      CZ = CZERO
      IF (AZ.GT.RTR1) CZ = HZ*HZ
      ACZ = CABS(CZ)
      NN = N
      CK = CLOG(HZ)
   10 CONTINUE
      DFNU = FNU + FLOAT(NN-1)
      FNUP = DFNU + 1.0E0
C-----------------------------------------------------------------------
C     UNDERFLOW TEST
C-----------------------------------------------------------------------
      AK1 = CK*CMPLX(DFNU,0.0E0)
      AK = GAMLN(FNUP,IDUM)
      AK1 = AK1 - CMPLX(AK,0.0E0)
      IF (KODE.EQ.2) AK1 = AK1 - CMPLX(X,0.0E0)
      RAK1 = REAL(AK1)
      IF (RAK1.GT.(-ELIM)) GO TO 30
   20 CONTINUE
      NZ = NZ + 1
      Y(NN) = CZERO
      IF (ACZ.GT.DFNU) GO TO 170
      NN = NN - 1
      IF (NN.EQ.0) RETURN
      GO TO 10
   30 CONTINUE
      IF (RAK1.GT.(-ALIM)) GO TO 40
      IFLAG = 1
      SS = 1.0E0/TOL
      CRSC = CMPLX(TOL,0.0E0)
      ASCLE = ARM*SS
   40 CONTINUE
      AK = AIMAG(AK1)
      AA = EXP(RAK1)
      IF (IFLAG.EQ.1) AA = AA*SS
      COEF = CMPLX(AA,0.0E0)*CMPLX(COS(AK),SIN(AK))
      ATOL = TOL*ACZ/FNUP
      IL = MIN0(2,NN)
      DO 80 I=1,IL
        DFNU = FNU + FLOAT(NN-I)
        FNUP = DFNU + 1.0E0
        S1 = CONE
        IF (ACZ.LT.TOL*FNUP) GO TO 60
        AK1 = CONE
        AK = FNUP + 2.0E0
        S = FNUP
        AA = 2.0E0
   50   CONTINUE
        RS = 1.0E0/S
        AK1 = AK1*CZ*CMPLX(RS,0.0E0)
        S1 = S1 + AK1
        S = S + AK
        AK = AK + 2.0E0
        AA = AA*ACZ*RS
        IF (AA.GT.ATOL) GO TO 50
   60   CONTINUE
        M = NN - I + 1
        S2 = S1*COEF
        W(I) = S2
        IF (IFLAG.EQ.0) GO TO 70
        CALL CUCHK(S2, NW, ASCLE, TOL)
        IF (NW.NE.0) GO TO 20
   70   CONTINUE
        Y(M) = S2*CRSC
        IF (I.NE.IL) COEF = COEF*CMPLX(DFNU,0.0E0)/HZ
   80 CONTINUE
      IF (NN.LE.2) RETURN
      K = NN - 2
      AK = FLOAT(K)
      RZ = (CONE+CONE)/Z
      IF (IFLAG.EQ.1) GO TO 110
      IB = 3
   90 CONTINUE
      DO 100 I=IB,NN
        Y(K) = CMPLX(AK+FNU,0.0E0)*RZ*Y(K+1) + Y(K+2)
        AK = AK - 1.0E0
        K = K - 1
  100 CONTINUE
      RETURN
C-----------------------------------------------------------------------
C     RECUR BACKWARD WITH SCALED VALUES
C-----------------------------------------------------------------------
  110 CONTINUE
C-----------------------------------------------------------------------
C     EXP(-ALIM)=EXP(-ELIM)/TOL=APPROX. ONE PRECISION ABOVE THE
C     UNDERFLOW LIMIT = ASCLE = R1MACH(1)*CSCL*1.0E+3
C-----------------------------------------------------------------------
      S1 = W(1)
      S2 = W(2)
      DO 120 L=3,NN
        CK = S2
        S2 = S1 + CMPLX(AK+FNU,0.0E0)*RZ*S2
        S1 = CK
        CK = S2*CRSC
        Y(K) = CK
        AK = AK - 1.0E0
        K = K - 1
        IF (CABS(CK).GT.ASCLE) GO TO 130
  120 CONTINUE
      RETURN
  130 CONTINUE
      IB = L + 1
      IF (IB.GT.NN) RETURN
      GO TO 90
  140 CONTINUE
      NZ = N
      IF (FNU.EQ.0.0E0) NZ = NZ - 1
  150 CONTINUE
      Y(1) = CZERO
      IF (FNU.EQ.0.0E0) Y(1) = CONE
      IF (N.EQ.1) RETURN
      DO 160 I=2,N
        Y(I) = CZERO
  160 CONTINUE
      RETURN
C-----------------------------------------------------------------------
C     RETURN WITH NZ.LT.0 IF CABS(Z*Z/4).GT.FNU+N-NZ-1 COMPLETE
C     THE CALCULATION IN CBINU WITH N=N-IABS(NZ)
C-----------------------------------------------------------------------
  170 CONTINUE
      NZ = -NZ
      RETURN
      END

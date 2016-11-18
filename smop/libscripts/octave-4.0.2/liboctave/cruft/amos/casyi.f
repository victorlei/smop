      SUBROUTINE CASYI(Z, FNU, KODE, N, Y, NZ, RL, TOL, ELIM, ALIM)
C***BEGIN PROLOGUE  CASYI
C***REFER TO  CBESI,CBESK
C
C     CASYI COMPUTES THE I BESSEL FUNCTION FOR REAL(Z).GE.0.0 BY
C     MEANS OF THE ASYMPTOTIC EXPANSION FOR LARGE CABS(Z) IN THE
C     REGION CABS(Z).GT.MAX(RL,FNU*FNU/2). NZ=0 IS A NORMAL RETURN.
C     NZ.LT.0 INDICATES AN OVERFLOW ON KODE=1.
C
C***ROUTINES CALLED  R1MACH
C***END PROLOGUE  CASYI
      COMPLEX AK1, CK, CONE, CS1, CS2, CZ, CZERO, DK, EZ, P1, RZ, S2,
     * Y, Z
      REAL AA, ACZ, AEZ, AK, ALIM, ARG, ARM, ATOL, AZ, BB, BK, DFNU,
     * DNU2, ELIM, FDN, FNU, PI, RL, RTPI, RTR1, S, SGN, SQK, TOL, X,
     * YY, R1MACH
      INTEGER I, IB, IL, INU, J, JL, K, KODE, KODED, M, N, NN, NZ
      DIMENSION Y(N)
      DATA PI, RTPI  /3.14159265358979324E0 , 0.159154943091895336E0 /
      DATA CZERO, CONE / (0.0E0,0.0E0), (1.0E0,0.0E0) /
C
      NZ = 0
      AZ = CABS(Z)
      X = REAL(Z)
      ARM = 1.0E+3*R1MACH(1)
      RTR1 = SQRT(ARM)
      IL = MIN0(2,N)
      DFNU = FNU + FLOAT(N-IL)
C-----------------------------------------------------------------------
C     OVERFLOW TEST
C-----------------------------------------------------------------------
      AK1 = CMPLX(RTPI,0.0E0)/Z
      AK1 = CSQRT(AK1)
      CZ = Z
      IF (KODE.EQ.2) CZ = Z - CMPLX(X,0.0E0)
      ACZ = REAL(CZ)
      IF (ABS(ACZ).GT.ELIM) GO TO 80
      DNU2 = DFNU + DFNU
      KODED = 1
      IF ((ABS(ACZ).GT.ALIM) .AND. (N.GT.2)) GO TO 10
      KODED = 0
      AK1 = AK1*CEXP(CZ)
   10 CONTINUE
      FDN = 0.0E0
      IF (DNU2.GT.RTR1) FDN = DNU2*DNU2
      EZ = Z*CMPLX(8.0E0,0.0E0)
C-----------------------------------------------------------------------
C     WHEN Z IS IMAGINARY, THE ERROR TEST MUST BE MADE RELATIVE TO THE
C     FIRST RECIPROCAL POWER SINCE THIS IS THE LEADING TERM OF THE
C     EXPANSION FOR THE IMAGINARY PART.
C-----------------------------------------------------------------------
      AEZ = 8.0E0*AZ
      S = TOL/AEZ
      JL = INT(RL+RL) + 2
      YY = AIMAG(Z)
      P1 = CZERO
      IF (YY.EQ.0.0E0) GO TO 20
C-----------------------------------------------------------------------
C     CALCULATE EXP(PI*(0.5+FNU+N-IL)*I) TO MINIMIZE LOSSES OF
C     SIGNIFICANCE WHEN FNU OR N IS LARGE
C-----------------------------------------------------------------------
      INU = INT(FNU)
      ARG = (FNU-FLOAT(INU))*PI
      INU = INU + N - IL
      AK = -SIN(ARG)
      BK = COS(ARG)
      IF (YY.LT.0.0E0) BK = -BK
      P1 = CMPLX(AK,BK)
      IF (MOD(INU,2).EQ.1) P1 = -P1
   20 CONTINUE
      DO 50 K=1,IL
        SQK = FDN - 1.0E0
        ATOL = S*ABS(SQK)
        SGN = 1.0E0
        CS1 = CONE
        CS2 = CONE
        CK = CONE
        AK = 0.0E0
        AA = 1.0E0
        BB = AEZ
        DK = EZ
        DO 30 J=1,JL
          CK = CK*CMPLX(SQK,0.0E0)/DK
          CS2 = CS2 + CK
          SGN = -SGN
          CS1 = CS1 + CK*CMPLX(SGN,0.0E0)
          DK = DK + EZ
          AA = AA*ABS(SQK)/BB
          BB = BB + AEZ
          AK = AK + 8.0E0
          SQK = SQK - AK
          IF (AA.LE.ATOL) GO TO 40
   30   CONTINUE
        GO TO 90
   40   CONTINUE
        S2 = CS1
        IF (X+X.LT.ELIM) S2 = S2 + P1*CS2*CEXP(-Z-Z)
        FDN = FDN + 8.0E0*DFNU + 4.0E0
        P1 = -P1
        M = N - IL + K
        Y(M) = S2*AK1
   50 CONTINUE
      IF (N.LE.2) RETURN
      NN = N
      K = NN - 2
      AK = FLOAT(K)
      RZ = (CONE+CONE)/Z
      IB = 3
      DO 60 I=IB,NN
        Y(K) = CMPLX(AK+FNU,0.0E0)*RZ*Y(K+1) + Y(K+2)
        AK = AK - 1.0E0
        K = K - 1
   60 CONTINUE
      IF (KODED.EQ.0) RETURN
      CK = CEXP(CZ)
      DO 70 I=1,NN
        Y(I) = Y(I)*CK
   70 CONTINUE
      RETURN
   80 CONTINUE
      NZ = -1
      RETURN
   90 CONTINUE
      NZ=-2
      RETURN
      END

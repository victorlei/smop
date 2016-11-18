      SUBROUTINE CMLRI(Z, FNU, KODE, N, Y, NZ, TOL)
C***BEGIN PROLOGUE  CMLRI
C***REFER TO  CBESI,CBESK
C
C     CMLRI COMPUTES THE I BESSEL FUNCTION FOR RE(Z).GE.0.0 BY THE
C     MILLER ALGORITHM NORMALIZED BY A NEUMANN SERIES.
C
C***ROUTINES CALLED  GAMLN,R1MACH
C***END PROLOGUE  CMLRI
      COMPLEX CK, CNORM, CONE, CTWO, CZERO, PT, P1, P2, RZ, SUM, Y, Z
      REAL ACK, AK, AP, AT, AZ, BK, FKAP, FKK, FLAM, FNF, FNU, RHO,
     * RHO2, SCLE, TFNF, TOL, TST, X, GAMLN, R1MACH
      INTEGER I, IAZ, IDUM, IFNU, INU, ITIME, K, KK, KM, KODE, M, N
      DIMENSION Y(N)
      DATA CZERO,CONE,CTWO /(0.0E0,0.0E0),(1.0E0,0.0E0),(2.0E0,0.0E0)/
      SCLE = 1.0E+3*R1MACH(1)/TOL
      NZ=0
      AZ = CABS(Z)
      X = REAL(Z)
      IAZ = INT(AZ)
      IFNU = INT(FNU)
      INU = IFNU + N - 1
      AT = FLOAT(IAZ) + 1.0E0
      CK = CMPLX(AT,0.0E0)/Z
      RZ = CTWO/Z
      P1 = CZERO
      P2 = CONE
      ACK = (AT+1.0E0)/AZ
      RHO = ACK + SQRT(ACK*ACK-1.0E0)
      RHO2 = RHO*RHO
      TST = (RHO2+RHO2)/((RHO2-1.0E0)*(RHO-1.0E0))
      TST = TST/TOL
C-----------------------------------------------------------------------
C     COMPUTE RELATIVE TRUNCATION ERROR INDEX FOR SERIES
C-----------------------------------------------------------------------
      AK = AT
      DO 10 I=1,80
        PT = P2
        P2 = P1 - CK*P2
        P1 = PT
        CK = CK + RZ
        AP = CABS(P2)
        IF (AP.GT.TST*AK*AK) GO TO 20
        AK = AK + 1.0E0
   10 CONTINUE
      GO TO 110
   20 CONTINUE
      I = I + 1
      K = 0
      IF (INU.LT.IAZ) GO TO 40
C-----------------------------------------------------------------------
C     COMPUTE RELATIVE TRUNCATION ERROR FOR RATIOS
C-----------------------------------------------------------------------
      P1 = CZERO
      P2 = CONE
      AT = FLOAT(INU) + 1.0E0
      CK = CMPLX(AT,0.0E0)/Z
      ACK = AT/AZ
      TST = SQRT(ACK/TOL)
      ITIME = 1
      DO 30 K=1,80
        PT = P2
        P2 = P1 - CK*P2
        P1 = PT
        CK = CK + RZ
        AP = CABS(P2)
        IF (AP.LT.TST) GO TO 30
        IF (ITIME.EQ.2) GO TO 40
        ACK = CABS(CK)
        FLAM = ACK + SQRT(ACK*ACK-1.0E0)
        FKAP = AP/CABS(P1)
        RHO = AMIN1(FLAM,FKAP)
        TST = TST*SQRT(RHO/(RHO*RHO-1.0E0))
        ITIME = 2
   30 CONTINUE
      GO TO 110
   40 CONTINUE
C-----------------------------------------------------------------------
C     BACKWARD RECURRENCE AND SUM NORMALIZING RELATION
C-----------------------------------------------------------------------
      K = K + 1
      KK = MAX0(I+IAZ,K+INU)
      FKK = FLOAT(KK)
      P1 = CZERO
C-----------------------------------------------------------------------
C     SCALE P2 AND SUM BY SCLE
C-----------------------------------------------------------------------
      P2 = CMPLX(SCLE,0.0E0)
      FNF = FNU - FLOAT(IFNU)
      TFNF = FNF + FNF
      BK = GAMLN(FKK+TFNF+1.0E0,IDUM) - GAMLN(FKK+1.0E0,IDUM)
     *     -GAMLN(TFNF+1.0E0,IDUM)
      BK = EXP(BK)
      SUM = CZERO
      KM = KK - INU
      DO 50 I=1,KM
        PT = P2
        P2 = P1 + CMPLX(FKK+FNF,0.0E0)*RZ*P2
        P1 = PT
        AK = 1.0E0 - TFNF/(FKK+TFNF)
        ACK = BK*AK
        SUM = SUM + CMPLX(ACK+BK,0.0E0)*P1
        BK = ACK
        FKK = FKK - 1.0E0
   50 CONTINUE
      Y(N) = P2
      IF (N.EQ.1) GO TO 70
      DO 60 I=2,N
        PT = P2
        P2 = P1 + CMPLX(FKK+FNF,0.0E0)*RZ*P2
        P1 = PT
        AK = 1.0E0 - TFNF/(FKK+TFNF)
        ACK = BK*AK
        SUM = SUM + CMPLX(ACK+BK,0.0E0)*P1
        BK = ACK
        FKK = FKK - 1.0E0
        M = N - I + 1
        Y(M) = P2
   60 CONTINUE
   70 CONTINUE
      IF (IFNU.LE.0) GO TO 90
      DO 80 I=1,IFNU
        PT = P2
        P2 = P1 + CMPLX(FKK+FNF,0.0E0)*RZ*P2
        P1 = PT
        AK = 1.0E0 - TFNF/(FKK+TFNF)
        ACK = BK*AK
        SUM = SUM + CMPLX(ACK+BK,0.0E0)*P1
        BK = ACK
        FKK = FKK - 1.0E0
   80 CONTINUE
   90 CONTINUE
      PT = Z
      IF (KODE.EQ.2) PT = PT - CMPLX(X,0.0E0)
      P1 = -CMPLX(FNF,0.0E0)*CLOG(RZ) + PT
      AP = GAMLN(1.0E0+FNF,IDUM)
      PT = P1 - CMPLX(AP,0.0E0)
C-----------------------------------------------------------------------
C     THE DIVISION CEXP(PT)/(SUM+P2) IS ALTERED TO AVOID OVERFLOW
C     IN THE DENOMINATOR BY SQUARING LARGE QUANTITIES
C-----------------------------------------------------------------------
      P2 = P2 + SUM
      AP = CABS(P2)
      P1 = CMPLX(1.0E0/AP,0.0E0)
      CK = CEXP(PT)*P1
      PT = CONJG(P2)*P1
      CNORM = CK*PT
      DO 100 I=1,N
        Y(I) = Y(I)*CNORM
  100 CONTINUE
      RETURN
  110 CONTINUE
      NZ=-2
      RETURN
      END

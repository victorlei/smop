      SUBROUTINE CKSCL(ZR, FNU, N, Y, NZ, RZ, ASCLE, TOL, ELIM)
C***BEGIN PROLOGUE  CKSCL
C***REFER TO  CBKNU,CUNK1,CUNK2
C
C     SET K FUNCTIONS TO ZERO ON UNDERFLOW, CONTINUE RECURRENCE
C     ON SCALED FUNCTIONS UNTIL TWO MEMBERS COME ON SCALE, THEN
C     RETURN WITH MIN(NZ+2,N) VALUES SCALED BY 1/TOL.
C
C***ROUTINES CALLED  CUCHK
C***END PROLOGUE  CKSCL
      COMPLEX CK, CS, CY, CZERO, RZ, S1, S2, Y, ZR, ZD, CELM
      REAL AA, ASCLE, ACS, AS, CSI, CSR, ELIM, FN, FNU, TOL, XX, ZRI,
     * ELM, ALAS, HELIM
      INTEGER I, IC, K, KK, N, NN, NW, NZ
      DIMENSION Y(N), CY(2)
      DATA CZERO / (0.0E0,0.0E0) /
C
      NZ = 0
      IC = 0
      XX = REAL(ZR)
      NN = MIN0(2,N)
      DO 10 I=1,NN
        S1 = Y(I)
        CY(I) = S1
        AS = CABS(S1)
        ACS = -XX + ALOG(AS)
        NZ = NZ + 1
        Y(I) = CZERO
        IF (ACS.LT.(-ELIM)) GO TO 10
        CS = -ZR + CLOG(S1)
        CSR = REAL(CS)
        CSI = AIMAG(CS)
        AA = EXP(CSR)/TOL
        CS = CMPLX(AA,0.0E0)*CMPLX(COS(CSI),SIN(CSI))
        CALL CUCHK(CS, NW, ASCLE, TOL)
        IF (NW.NE.0) GO TO 10
        Y(I) = CS
        NZ = NZ - 1
        IC = I
   10 CONTINUE
      IF (N.EQ.1) RETURN
      IF (IC.GT.1) GO TO 20
      Y(1) = CZERO
      NZ = 2
   20 CONTINUE
      IF (N.EQ.2) RETURN
      IF (NZ.EQ.0) RETURN
      FN = FNU + 1.0E0
      CK = CMPLX(FN,0.0E0)*RZ
      S1 = CY(1)
      S2 = CY(2)
      HELIM = 0.5E0*ELIM
      ELM = EXP(-ELIM)
      CELM = CMPLX(ELM,0.0E0)
      ZRI =AIMAG(ZR)
      ZD = ZR
C
C     FIND TWO CONSECUTIVE Y VALUES ON SCALE. SCALE RECURRENCE IF
C     S2 GETS LARGER THAN EXP(ELIM/2)
C
      DO 30 I=3,N
        KK = I
        CS = S2
        S2 = CK*S2 + S1
        S1 = CS
        CK = CK + RZ
        AS = CABS(S2)
        ALAS = ALOG(AS)
        ACS = -XX + ALAS
        NZ = NZ + 1
        Y(I) = CZERO
        IF (ACS.LT.(-ELIM)) GO TO 25
        CS = -ZD + CLOG(S2)
        CSR = REAL(CS)
        CSI = AIMAG(CS)
        AA = EXP(CSR)/TOL
        CS = CMPLX(AA,0.0E0)*CMPLX(COS(CSI),SIN(CSI))
        CALL CUCHK(CS, NW, ASCLE, TOL)
        IF (NW.NE.0) GO TO 25
        Y(I) = CS
        NZ = NZ - 1
        IF (IC.EQ.(KK-1)) GO TO 40
        IC = KK
        GO TO 30
   25   CONTINUE
        IF(ALAS.LT.HELIM) GO TO 30
        XX = XX-ELIM
        S1 = S1*CELM
        S2 = S2*CELM
        ZD = CMPLX(XX,ZRI)
   30 CONTINUE
      NZ = N
      IF(IC.EQ.N) NZ=N-1
      GO TO 45
   40 CONTINUE
      NZ = KK - 2
   45 CONTINUE
      DO 50 K=1,NZ
        Y(K) = CZERO
   50 CONTINUE
      RETURN
      END

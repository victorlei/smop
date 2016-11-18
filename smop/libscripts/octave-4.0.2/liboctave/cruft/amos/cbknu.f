      SUBROUTINE CBKNU(Z, FNU, KODE, N, Y, NZ, TOL, ELIM, ALIM)
C***BEGIN PROLOGUE  CBKNU
C***REFER TO  CBESI,CBESK,CAIRY,CBESH
C
C     CBKNU COMPUTES THE K BESSEL FUNCTION IN THE RIGHT HALF Z PLANE
C
C***ROUTINES CALLED  CKSCL,CSHCH,GAMLN,I1MACH,R1MACH,CUCHK
C***END PROLOGUE  CBKNU
C
      COMPLEX CCH, CK, COEF, CONE, CRSC, CS, CSCL, CSH, CSR, CSS, CTWO,
     * CZ, CZERO, F, FMU, P, PT, P1, P2, Q, RZ, SMU, ST, S1, S2, Y, Z,
     * ZD, CELM, CY
      REAL AA, AK, ALIM, ASCLE, A1, A2, BB, BK, BRY, CAZ, CC, DNU,
     * DNU2, ELIM, ETEST, FC, FHS, FK, FKS, FNU, FPI, G1, G2, HPI, PI,
     * P2I, P2M, P2R, RK, RTHPI, R1, S, SPI, TM, TOL, TTH, T1, T2, XX,
     * YY, GAMLN, R1MACH, HELIM, ELM, XD, YD, ALAS, AS
      INTEGER I, IDUM, IFLAG, INU, K, KFLAG, KK, KMAX, KODE, KODED, N,
     * NZ, I1MACH, NW, J, IC, INUB
      DIMENSION BRY(3), CC(8), CSS(3), CSR(3), Y(N), CY(2)
C
      DATA KMAX / 30 /
      DATA R1 / 2.0E0 /
      DATA CZERO,CONE,CTWO /(0.0E0,0.0E0),(1.0E0,0.0E0),(2.0E0,0.0E0)/
C
      DATA PI, RTHPI, SPI ,HPI, FPI, TTH /
     1     3.14159265358979324E0,       1.25331413731550025E0,
     2     1.90985931710274403E0,       1.57079632679489662E0,
     3     1.89769999331517738E0,       6.66666666666666666E-01/
C
      DATA CC(1), CC(2), CC(3), CC(4), CC(5), CC(6), CC(7), CC(8)/
     1     5.77215664901532861E-01,    -4.20026350340952355E-02,
     2    -4.21977345555443367E-02,     7.21894324666309954E-03,
     3    -2.15241674114950973E-04,    -2.01348547807882387E-05,
     4     1.13302723198169588E-06,     6.11609510448141582E-09/
C
      XX = REAL(Z)
      YY = AIMAG(Z)
      CAZ = CABS(Z)
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
      NZ = 0
      IFLAG = 0
      KODED = KODE
      RZ = CTWO/Z
      INU = INT(FNU+0.5E0)
      DNU = FNU - FLOAT(INU)
      IF (ABS(DNU).EQ.0.5E0) GO TO 110
      DNU2 = 0.0E0
      IF (ABS(DNU).GT.TOL) DNU2 = DNU*DNU
      IF (CAZ.GT.R1) GO TO 110
C-----------------------------------------------------------------------
C     SERIES FOR CABS(Z).LE.R1
C-----------------------------------------------------------------------
      FC = 1.0E0
      SMU = CLOG(RZ)
      FMU = SMU*CMPLX(DNU,0.0E0)
      CALL CSHCH(FMU, CSH, CCH)
      IF (DNU.EQ.0.0E0) GO TO 10
      FC = DNU*PI
      FC = FC/SIN(FC)
      SMU = CSH*CMPLX(1.0E0/DNU,0.0E0)
   10 CONTINUE
      A2 = 1.0E0 + DNU
C-----------------------------------------------------------------------
C     GAM(1-Z)*GAM(1+Z)=PI*Z/SIN(PI*Z), T1=1/GAM(1-DNU), T2=1/GAM(1+DNU)
C-----------------------------------------------------------------------
      T2 = EXP(-GAMLN(A2,IDUM))
      T1 = 1.0E0/(T2*FC)
      IF (ABS(DNU).GT.0.1E0) GO TO 40
C-----------------------------------------------------------------------
C     SERIES FOR F0 TO RESOLVE INDETERMINACY FOR SMALL ABS(DNU)
C-----------------------------------------------------------------------
      AK = 1.0E0
      S = CC(1)
      DO 20 K=2,8
        AK = AK*DNU2
        TM = CC(K)*AK
        S = S + TM
        IF (ABS(TM).LT.TOL) GO TO 30
   20 CONTINUE
   30 G1 = -S
      GO TO 50
   40 CONTINUE
      G1 = (T1-T2)/(DNU+DNU)
   50 CONTINUE
      G2 = 0.5E0*(T1+T2)*FC
      G1 = G1*FC
      F = CMPLX(G1,0.0E0)*CCH + SMU*CMPLX(G2,0.0E0)
      PT = CEXP(FMU)
      P = CMPLX(0.5E0/T2,0.0E0)*PT
      Q = CMPLX(0.5E0/T1,0.0E0)/PT
      S1 = F
      S2 = P
      AK = 1.0E0
      A1 = 1.0E0
      CK = CONE
      BK = 1.0E0 - DNU2
      IF (INU.GT.0 .OR. N.GT.1) GO TO 80
C-----------------------------------------------------------------------
C     GENERATE K(FNU,Z), 0.0D0 .LE. FNU .LT. 0.5D0 AND N=1
C-----------------------------------------------------------------------
      IF (CAZ.LT.TOL) GO TO 70
      CZ = Z*Z*CMPLX(0.25E0,0.0E0)
      T1 = 0.25E0*CAZ*CAZ
   60 CONTINUE
      F = (F*CMPLX(AK,0.0E0)+P+Q)*CMPLX(1.0E0/BK,0.0E0)
      P = P*CMPLX(1.0E0/(AK-DNU),0.0E0)
      Q = Q*CMPLX(1.0E0/(AK+DNU),0.0E0)
      RK = 1.0E0/AK
      CK = CK*CZ*CMPLX(RK,0.0)
      S1 = S1 + CK*F
      A1 = A1*T1*RK
      BK = BK + AK + AK + 1.0E0
      AK = AK + 1.0E0
      IF (A1.GT.TOL) GO TO 60
   70 CONTINUE
      Y(1) = S1
      IF (KODED.EQ.1) RETURN
      Y(1) = S1*CEXP(Z)
      RETURN
C-----------------------------------------------------------------------
C     GENERATE K(DNU,Z) AND K(DNU+1,Z) FOR FORWARD RECURRENCE
C-----------------------------------------------------------------------
   80 CONTINUE
      IF (CAZ.LT.TOL) GO TO 100
      CZ = Z*Z*CMPLX(0.25E0,0.0E0)
      T1 = 0.25E0*CAZ*CAZ
   90 CONTINUE
      F = (F*CMPLX(AK,0.0E0)+P+Q)*CMPLX(1.0E0/BK,0.0E0)
      P = P*CMPLX(1.0E0/(AK-DNU),0.0E0)
      Q = Q*CMPLX(1.0E0/(AK+DNU),0.0E0)
      RK = 1.0E0/AK
      CK = CK*CZ*CMPLX(RK,0.0E0)
      S1 = S1 + CK*F
      S2 = S2 + CK*(P-F*CMPLX(AK,0.0E0))
      A1 = A1*T1*RK
      BK = BK + AK + AK + 1.0E0
      AK = AK + 1.0E0
      IF (A1.GT.TOL) GO TO 90
  100 CONTINUE
      KFLAG = 2
      BK = REAL(SMU)
      A1 = FNU + 1.0E0
      AK = A1*ABS(BK)
      IF (AK.GT.ALIM) KFLAG = 3
      P2 = S2*CSS(KFLAG)
      S2 = P2*RZ
      S1 = S1*CSS(KFLAG)
      IF (KODED.EQ.1) GO TO 210
      F = CEXP(Z)
      S1 = S1*F
      S2 = S2*F
      GO TO 210
C-----------------------------------------------------------------------
C     IFLAG=0 MEANS NO UNDERFLOW OCCURRED
C     IFLAG=1 MEANS AN UNDERFLOW OCCURRED- COMPUTATION PROCEEDS WITH
C     KODED=2 AND A TEST FOR ON SCALE VALUES IS MADE DURING FORWARD
C     RECURSION
C-----------------------------------------------------------------------
  110 CONTINUE
      COEF = CMPLX(RTHPI,0.0E0)/CSQRT(Z)
      KFLAG = 2
      IF (KODED.EQ.2) GO TO 120
      IF (XX.GT.ALIM) GO TO 290
C     BLANK LINE
      A1 = EXP(-XX)*REAL(CSS(KFLAG))
      PT = CMPLX(A1,0.0E0)*CMPLX(COS(YY),-SIN(YY))
      COEF = COEF*PT
  120 CONTINUE
      IF (ABS(DNU).EQ.0.5E0) GO TO 300
C-----------------------------------------------------------------------
C     MILLER ALGORITHM FOR CABS(Z).GT.R1
C-----------------------------------------------------------------------
      AK = COS(PI*DNU)
      AK = ABS(AK)
      IF (AK.EQ.0.0E0) GO TO 300
      FHS = ABS(0.25E0-DNU2)
      IF (FHS.EQ.0.0E0) GO TO 300
C-----------------------------------------------------------------------
C     COMPUTE R2=F(E). IF CABS(Z).GE.R2, USE FORWARD RECURRENCE TO
C     DETERMINE THE BACKWARD INDEX K. R2=F(E) IS A STRAIGHT LINE ON
C     12.LE.E.LE.60. E IS COMPUTED FROM 2**(-E)=B**(1-I1MACH(11))=
C     TOL WHERE B IS THE BASE OF THE ARITHMETIC.
C-----------------------------------------------------------------------
      T1 = FLOAT(I1MACH(11)-1)*R1MACH(5)*3.321928094E0
      T1 = AMAX1(T1,12.0E0)
      T1 = AMIN1(T1,60.0E0)
      T2 = TTH*T1 - 6.0E0
      IF (XX.NE.0.0E0) GO TO 130
      T1 = HPI
      GO TO 140
  130 CONTINUE
      T1 = ATAN(YY/XX)
      T1 = ABS(T1)
  140 CONTINUE
      IF (T2.GT.CAZ) GO TO 170
C-----------------------------------------------------------------------
C     FORWARD RECURRENCE LOOP WHEN CABS(Z).GE.R2
C-----------------------------------------------------------------------
      ETEST = AK/(PI*CAZ*TOL)
      FK = 1.0E0
      IF (ETEST.LT.1.0E0) GO TO 180
      FKS = 2.0E0
      RK = CAZ + CAZ + 2.0E0
      A1 = 0.0E0
      A2 = 1.0E0
      DO 150 I=1,KMAX
        AK = FHS/FKS
        BK = RK/(FK+1.0E0)
        TM = A2
        A2 = BK*A2 - AK*A1
        A1 = TM
        RK = RK + 2.0E0
        FKS = FKS + FK + FK + 2.0E0
        FHS = FHS + FK + FK
        FK = FK + 1.0E0
        TM = ABS(A2)*FK
        IF (ETEST.LT.TM) GO TO 160
  150 CONTINUE
      GO TO 310
  160 CONTINUE
      FK = FK + SPI*T1*SQRT(T2/CAZ)
      FHS = ABS(0.25E0-DNU2)
      GO TO 180
  170 CONTINUE
C-----------------------------------------------------------------------
C     COMPUTE BACKWARD INDEX K FOR CABS(Z).LT.R2
C-----------------------------------------------------------------------
      A2 = SQRT(CAZ)
      AK = FPI*AK/(TOL*SQRT(A2))
      AA = 3.0E0*T1/(1.0E0+CAZ)
      BB = 14.7E0*T1/(28.0E0+CAZ)
      AK = (ALOG(AK)+CAZ*COS(AA)/(1.0E0+0.008E0*CAZ))/COS(BB)
      FK = 0.12125E0*AK*AK/CAZ + 1.5E0
  180 CONTINUE
      K = INT(FK)
C-----------------------------------------------------------------------
C     BACKWARD RECURRENCE LOOP FOR MILLER ALGORITHM
C-----------------------------------------------------------------------
      FK = FLOAT(K)
      FKS = FK*FK
      P1 = CZERO
      P2 = CMPLX(TOL,0.0E0)
      CS = P2
      DO 190 I=1,K
        A1 = FKS - FK
        A2 = (FKS+FK)/(A1+FHS)
        RK = 2.0E0/(FK+1.0E0)
        T1 = (FK+XX)*RK
        T2 = YY*RK
        PT = P2
        P2 = (P2*CMPLX(T1,T2)-P1)*CMPLX(A2,0.0E0)
        P1 = PT
        CS = CS + P2
        FKS = A1 - FK + 1.0E0
        FK = FK - 1.0E0
  190 CONTINUE
C-----------------------------------------------------------------------
C     COMPUTE (P2/CS)=(P2/CABS(CS))*(CONJG(CS)/CABS(CS)) FOR BETTER
C     SCALING
C-----------------------------------------------------------------------
      TM = CABS(CS)
      PT = CMPLX(1.0E0/TM,0.0E0)
      S1 = PT*P2
      CS = CONJG(CS)*PT
      S1 = COEF*S1*CS
      IF (INU.GT.0 .OR. N.GT.1) GO TO 200
      ZD = Z
      IF(IFLAG.EQ.1) GO TO 270
      GO TO 240
  200 CONTINUE
C-----------------------------------------------------------------------
C     COMPUTE P1/P2=(P1/CABS(P2)*CONJG(P2)/CABS(P2) FOR SCALING
C-----------------------------------------------------------------------
      TM = CABS(P2)
      PT = CMPLX(1.0E0/TM,0.0E0)
      P1 = PT*P1
      P2 = CONJG(P2)*PT
      PT = P1*P2
      S2 = S1*(CONE+(CMPLX(DNU+0.5E0,0.0E0)-PT)/Z)
C-----------------------------------------------------------------------
C     FORWARD RECURSION ON THE THREE TERM RECURSION RELATION WITH
C     SCALING NEAR EXPONENT EXTREMES ON KFLAG=1 OR KFLAG=3
C-----------------------------------------------------------------------
  210 CONTINUE
      CK = CMPLX(DNU+1.0E0,0.0E0)*RZ
      IF (N.EQ.1) INU = INU - 1
      IF (INU.GT.0) GO TO 220
      IF (N.EQ.1) S1=S2
      ZD = Z
      IF(IFLAG.EQ.1) GO TO 270
      GO TO 240
  220 CONTINUE
      INUB = 1
      IF (IFLAG.EQ.1) GO TO 261
  225 CONTINUE
      P1 = CSR(KFLAG)
      ASCLE = BRY(KFLAG)
      DO 230 I=INUB,INU
        ST = S2
        S2 = CK*S2 + S1
        S1 = ST
        CK = CK + RZ
        IF (KFLAG.GE.3) GO TO 230
        P2 = S2*P1
        P2R = REAL(P2)
        P2I = AIMAG(P2)
        P2R = ABS(P2R)
        P2I = ABS(P2I)
        P2M = AMAX1(P2R,P2I)
        IF (P2M.LE.ASCLE) GO TO 230
        KFLAG = KFLAG + 1
        ASCLE = BRY(KFLAG)
        S1 = S1*P1
        S2 = P2
        S1 = S1*CSS(KFLAG)
        S2 = S2*CSS(KFLAG)
        P1 = CSR(KFLAG)
  230 CONTINUE
      IF (N.EQ.1) S1 = S2
  240 CONTINUE
      Y(1) = S1*CSR(KFLAG)
      IF (N.EQ.1) RETURN
      Y(2) = S2*CSR(KFLAG)
      IF (N.EQ.2) RETURN
      KK = 2
  250 CONTINUE
      KK = KK + 1
      IF (KK.GT.N) RETURN
      P1 = CSR(KFLAG)
      ASCLE = BRY(KFLAG)
      DO 260 I=KK,N
        P2 = S2
        S2 = CK*S2 + S1
        S1 = P2
        CK = CK + RZ
        P2 = S2*P1
        Y(I) = P2
        IF (KFLAG.GE.3) GO TO 260
        P2R = REAL(P2)
        P2I = AIMAG(P2)
        P2R = ABS(P2R)
        P2I = ABS(P2I)
        P2M = AMAX1(P2R,P2I)
        IF (P2M.LE.ASCLE) GO TO 260
        KFLAG = KFLAG + 1
        ASCLE = BRY(KFLAG)
        S1 = S1*P1
        S2 = P2
        S1 = S1*CSS(KFLAG)
        S2 = S2*CSS(KFLAG)
        P1 = CSR(KFLAG)
  260 CONTINUE
      RETURN
C-----------------------------------------------------------------------
C     IFLAG=1 CASES, FORWARD RECURRENCE ON SCALED VALUES ON UNDERFLOW
C-----------------------------------------------------------------------
  261 CONTINUE
      HELIM = 0.5E0*ELIM
      ELM = EXP(-ELIM)
      CELM = CMPLX(ELM,0.0)
      ASCLE = BRY(1)
      ZD = Z
      XD = XX
      YD = YY
      IC = -1
      J = 2
      DO 262 I=1,INU
        ST = S2
        S2 = CK*S2+S1
        S1 = ST
        CK = CK+RZ
        AS = CABS(S2)
        ALAS = ALOG(AS)
        P2R = -XD+ALAS
        IF(P2R.LT.(-ELIM)) GO TO 263
        P2 = -ZD+CLOG(S2)
        P2R = REAL(P2)
        P2I = AIMAG(P2)
        P2M = EXP(P2R)/TOL
        P1 = CMPLX(P2M,0.0E0)*CMPLX(COS(P2I),SIN(P2I))
        CALL CUCHK(P1,NW,ASCLE,TOL)
        IF(NW.NE.0) GO TO 263
        J=3-J
        CY(J) = P1
        IF(IC.EQ.(I-1)) GO TO 264
        IC = I
        GO TO 262
  263   CONTINUE
        IF(ALAS.LT.HELIM) GO TO 262
        XD = XD-ELIM
        S1 = S1*CELM
        S2 = S2*CELM
        ZD = CMPLX(XD,YD)
  262 CONTINUE
      IF(N.EQ.1) S1 = S2
      GO TO 270
  264 CONTINUE
      KFLAG = 1
      INUB = I+1
      S2 = CY(J)
      J = 3 - J
      S1 = CY(J)
      IF(INUB.LE.INU) GO TO 225
      IF(N.EQ.1) S1 = S2
      GO TO 240
  270 CONTINUE
      Y(1) = S1
      IF (N.EQ.1) GO TO 280
      Y(2) = S2
  280 CONTINUE
      ASCLE = BRY(1)
      CALL CKSCL(ZD, FNU, N, Y, NZ, RZ, ASCLE, TOL, ELIM)
      INU = N - NZ
      IF (INU.LE.0) RETURN
      KK = NZ + 1
      S1 = Y(KK)
      Y(KK) = S1*CSR(1)
      IF (INU.EQ.1) RETURN
      KK = NZ + 2
      S2 = Y(KK)
      Y(KK) = S2*CSR(1)
      IF (INU.EQ.2) RETURN
      T2 = FNU + FLOAT(KK-1)
      CK = CMPLX(T2,0.0E0)*RZ
      KFLAG = 1
      GO TO 250
  290 CONTINUE
C-----------------------------------------------------------------------
C     SCALE BY EXP(Z), IFLAG = 1 CASES
C-----------------------------------------------------------------------
      KODED = 2
      IFLAG = 1
      KFLAG = 2
      GO TO 120
C-----------------------------------------------------------------------
C     FNU=HALF ODD INTEGER CASE, DNU=-0.5
C-----------------------------------------------------------------------
  300 CONTINUE
      S1 = COEF
      S2 = COEF
      GO TO 210
  310 CONTINUE
      NZ=-2
      RETURN
      END

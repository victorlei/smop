      SUBROUTINE CACON(Z, FNU, KODE, MR, N, Y, NZ, RL, FNUL, TOL, ELIM,
     * ALIM)
C***BEGIN PROLOGUE  CACON
C***REFER TO  CBESK,CBESH
C
C     CACON APPLIES THE ANALYTIC CONTINUATION FORMULA
C
C         K(FNU,ZN*EXP(MP))=K(FNU,ZN)*EXP(-MP*FNU) - MP*I(FNU,ZN)
C                 MP=PI*MR*CMPLX(0.0,1.0)
C
C     TO CONTINUE THE K FUNCTION FROM THE RIGHT HALF TO THE LEFT
C     HALF Z PLANE
C
C***ROUTINES CALLED  CBINU,CBKNU,CS1S2,R1MACH
C***END PROLOGUE  CACON
      COMPLEX CK, CONE, CS, CSCL, CSCR, CSGN, CSPN, CSS, CSR, C1, C2,
     * RZ, SC1, SC2, ST, S1, S2, Y, Z, ZN, CY
      REAL ALIM, ARG, ASCLE, AS2, BSCLE, BRY, CPN, C1I, C1M, C1R, ELIM,
     * FMR, FNU, FNUL, PI, RL, SGN, SPN, TOL, YY, R1MACH
      INTEGER I, INU, IUF, KFLAG, KODE, MR, N, NN, NW, NZ
      DIMENSION Y(N), CY(2), CSS(3), CSR(3), BRY(3)
      DATA PI / 3.14159265358979324E0 /
      DATA CONE / (1.0E0,0.0E0) /
      NZ = 0
      ZN = -Z
      NN = N
      CALL CBINU(ZN, FNU, KODE, NN, Y, NW, RL, FNUL, TOL, ELIM, ALIM)
      IF (NW.LT.0) GO TO 80
C-----------------------------------------------------------------------
C     ANALYTIC CONTINUATION TO THE LEFT HALF PLANE FOR THE K FUNCTION
C-----------------------------------------------------------------------
      NN = MIN0(2,N)
      CALL CBKNU(ZN, FNU, KODE, NN, CY, NW, TOL, ELIM, ALIM)
      IF (NW.NE.0) GO TO 80
      S1 = CY(1)
      FMR = FLOAT(MR)
      SGN = -SIGN(PI,FMR)
      CSGN = CMPLX(0.0E0,SGN)
      IF (KODE.EQ.1) GO TO 10
      YY = -AIMAG(ZN)
      CPN = COS(YY)
      SPN = SIN(YY)
      CSGN = CSGN*CMPLX(CPN,SPN)
   10 CONTINUE
C-----------------------------------------------------------------------
C     CALCULATE CSPN=EXP(FNU*PI*I) TO MINIMIZE LOSSES OF SIGNIFICANCE
C     WHEN FNU IS LARGE
C-----------------------------------------------------------------------
      INU = INT(FNU)
      ARG = (FNU-FLOAT(INU))*SGN
      CPN = COS(ARG)
      SPN = SIN(ARG)
      CSPN = CMPLX(CPN,SPN)
      IF (MOD(INU,2).EQ.1) CSPN = -CSPN
      IUF = 0
      C1 = S1
      C2 = Y(1)
      ASCLE = 1.0E+3*R1MACH(1)/TOL
      IF (KODE.EQ.1) GO TO 20
      CALL CS1S2(ZN, C1, C2, NW, ASCLE, ALIM, IUF)
      NZ = NZ + NW
      SC1 = C1
   20 CONTINUE
      Y(1) = CSPN*C1 + CSGN*C2
      IF (N.EQ.1) RETURN
      CSPN = -CSPN
      S2 = CY(2)
      C1 = S2
      C2 = Y(2)
      IF (KODE.EQ.1) GO TO 30
      CALL CS1S2(ZN, C1, C2, NW, ASCLE, ALIM, IUF)
      NZ = NZ + NW
      SC2 = C1
   30 CONTINUE
      Y(2) = CSPN*C1 + CSGN*C2
      IF (N.EQ.2) RETURN
      CSPN = -CSPN
      RZ = CMPLX(2.0E0,0.0E0)/ZN
      CK = CMPLX(FNU+1.0E0,0.0E0)*RZ
C-----------------------------------------------------------------------
C     SCALE NEAR EXPONENT EXTREMES DURING RECURRENCE ON K FUNCTIONS
C-----------------------------------------------------------------------
      CSCL = CMPLX(1.0E0/TOL,0.0E0)
      CSCR = CMPLX(TOL,0.0E0)
      CSS(1) = CSCL
      CSS(2) = CONE
      CSS(3) = CSCR
      CSR(1) = CSCR
      CSR(2) = CONE
      CSR(3) = CSCL
      BRY(1) = ASCLE
      BRY(2) = 1.0E0/ASCLE
      BRY(3) = R1MACH(2)
      AS2 = CABS(S2)
      KFLAG = 2
      IF (AS2.GT.BRY(1)) GO TO 40
      KFLAG = 1
      GO TO 50
   40 CONTINUE
      IF (AS2.LT.BRY(2)) GO TO 50
      KFLAG = 3
   50 CONTINUE
      BSCLE = BRY(KFLAG)
      S1 = S1*CSS(KFLAG)
      S2 = S2*CSS(KFLAG)
      CS = CSR(KFLAG)
      DO 70 I=3,N
        ST = S2
        S2 = CK*S2 + S1
        S1 = ST
        C1 = S2*CS
        ST = C1
        C2 = Y(I)
        IF (KODE.EQ.1) GO TO 60
        IF (IUF.LT.0) GO TO 60
        CALL CS1S2(ZN, C1, C2, NW, ASCLE, ALIM, IUF)
        NZ = NZ + NW
        SC1 = SC2
        SC2 = C1
        IF (IUF.NE.3) GO TO 60
        IUF = -4
        S1 = SC1*CSS(KFLAG)
        S2 = SC2*CSS(KFLAG)
        ST = SC2
   60   CONTINUE
        Y(I) = CSPN*C1 + CSGN*C2
        CK = CK + RZ
        CSPN = -CSPN
        IF (KFLAG.GE.3) GO TO 70
        C1R = REAL(C1)
        C1I = AIMAG(C1)
        C1R = ABS(C1R)
        C1I = ABS(C1I)
        C1M = AMAX1(C1R,C1I)
        IF (C1M.LE.BSCLE) GO TO 70
        KFLAG = KFLAG + 1
        BSCLE = BRY(KFLAG)
        S1 = S1*CS
        S2 = ST
        S1 = S1*CSS(KFLAG)
        S2 = S2*CSS(KFLAG)
        CS = CSR(KFLAG)
   70 CONTINUE
      RETURN
   80 CONTINUE
      NZ = -1
      IF(NW.EQ.(-2)) NZ=-2
      RETURN
      END

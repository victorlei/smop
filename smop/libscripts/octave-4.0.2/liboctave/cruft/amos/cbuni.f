      SUBROUTINE CBUNI(Z, FNU, KODE, N, Y, NZ, NUI, NLAST, FNUL, TOL,
     * ELIM, ALIM)
C***BEGIN PROLOGUE  CBUNI
C***REFER TO  CBESI,CBESK
C
C     CBUNI COMPUTES THE I BESSEL FUNCTION FOR LARGE CABS(Z).GT.
C     FNUL AND FNU+N-1.LT.FNUL. THE ORDER IS INCREASED FROM
C     FNU+N-1 GREATER THAN FNUL BY ADDING NUI AND COMPUTING
C     ACCORDING TO THE UNIFORM ASYMPTOTIC EXPANSION FOR I(FNU,Z)
C     ON IFORM=1 AND THE EXPANSION FOR J(FNU,Z) ON IFORM=2
C
C***ROUTINES CALLED  CUNI1,CUNI2,R1MACH
C***END PROLOGUE  CBUNI
      COMPLEX CSCL, CSCR, CY, RZ, ST, S1, S2, Y, Z
      REAL ALIM, AX, AY, DFNU, ELIM, FNU, FNUI, FNUL, GNU, TOL, XX, YY,
     * ASCLE, BRY, STR, STI, STM, R1MACH
      INTEGER I, IFLAG, IFORM, K, KODE, N, NL, NLAST, NUI, NW, NZ
      DIMENSION Y(N), CY(2), BRY(3)
      NZ = 0
      XX = REAL(Z)
      YY = AIMAG(Z)
      AX = ABS(XX)*1.7321E0
      AY = ABS(YY)
      IFORM = 1
      IF (AY.GT.AX) IFORM = 2
      IF (NUI.EQ.0) GO TO 60
      FNUI = FLOAT(NUI)
      DFNU = FNU + FLOAT(N-1)
      GNU = DFNU + FNUI
      IF (IFORM.EQ.2) GO TO 10
C-----------------------------------------------------------------------
C     ASYMPTOTIC EXPANSION FOR I(FNU,Z) FOR LARGE FNU APPLIED IN
C     -PI/3.LE.ARG(Z).LE.PI/3
C-----------------------------------------------------------------------
      CALL CUNI1(Z, GNU, KODE, 2, CY, NW, NLAST, FNUL, TOL, ELIM, ALIM)
      GO TO 20
   10 CONTINUE
C-----------------------------------------------------------------------
C     ASYMPTOTIC EXPANSION FOR J(FNU,Z*EXP(M*HPI)) FOR LARGE FNU
C     APPLIED IN PI/3.LT.ABS(ARG(Z)).LE.PI/2 WHERE M=+I OR -I
C     AND HPI=PI/2
C-----------------------------------------------------------------------
      CALL CUNI2(Z, GNU, KODE, 2, CY, NW, NLAST, FNUL, TOL, ELIM, ALIM)
   20 CONTINUE
      IF (NW.LT.0) GO TO 50
      IF (NW.NE.0) GO TO 90
      AY = CABS(CY(1))
C----------------------------------------------------------------------
C     SCALE BACKWARD RECURRENCE, BRY(3) IS DEFINED BUT NEVER USED
C----------------------------------------------------------------------
      BRY(1) = 1.0E+3*R1MACH(1)/TOL
      BRY(2) = 1.0E0/BRY(1)
      BRY(3) = BRY(2)
      IFLAG = 2
      ASCLE = BRY(2)
      AX = 1.0E0
      CSCL = CMPLX(AX,0.0E0)
      IF (AY.GT.BRY(1)) GO TO 21
      IFLAG = 1
      ASCLE = BRY(1)
      AX = 1.0E0/TOL
      CSCL = CMPLX(AX,0.0E0)
      GO TO 25
   21 CONTINUE
      IF (AY.LT.BRY(2)) GO TO 25
      IFLAG = 3
      ASCLE = BRY(3)
      AX = TOL
      CSCL = CMPLX(AX,0.0E0)
   25 CONTINUE
      AY = 1.0E0/AX
      CSCR = CMPLX(AY,0.0E0)
      S1 = CY(2)*CSCL
      S2 = CY(1)*CSCL
      RZ = CMPLX(2.0E0,0.0E0)/Z
      DO 30 I=1,NUI
        ST = S2
        S2 = CMPLX(DFNU+FNUI,0.0E0)*RZ*S2 + S1
        S1 = ST
        FNUI = FNUI - 1.0E0
        IF (IFLAG.GE.3) GO TO 30
        ST = S2*CSCR
        STR = REAL(ST)
        STI = AIMAG(ST)
        STR = ABS(STR)
        STI = ABS(STI)
        STM = AMAX1(STR,STI)
        IF (STM.LE.ASCLE) GO TO 30
        IFLAG = IFLAG+1
        ASCLE = BRY(IFLAG)
        S1 = S1*CSCR
        S2 = ST
        AX = AX*TOL
        AY = 1.0E0/AX
        CSCL = CMPLX(AX,0.0E0)
        CSCR = CMPLX(AY,0.0E0)
        S1 = S1*CSCL
        S2 = S2*CSCL
   30 CONTINUE
      Y(N) = S2*CSCR
      IF (N.EQ.1) RETURN
      NL = N - 1
      FNUI = FLOAT(NL)
      K = NL
      DO 40 I=1,NL
        ST = S2
        S2 = CMPLX(FNU+FNUI,0.0E0)*RZ*S2 + S1
        S1 = ST
        ST = S2*CSCR
        Y(K) = ST
        FNUI = FNUI - 1.0E0
        K = K - 1
        IF (IFLAG.GE.3) GO TO 40
        STR = REAL(ST)
        STI = AIMAG(ST)
        STR = ABS(STR)
        STI = ABS(STI)
        STM = AMAX1(STR,STI)
        IF (STM.LE.ASCLE) GO TO 40
        IFLAG = IFLAG+1
        ASCLE = BRY(IFLAG)
        S1 = S1*CSCR
        S2 = ST
        AX = AX*TOL
        AY = 1.0E0/AX
        CSCL = CMPLX(AX,0.0E0)
        CSCR = CMPLX(AY,0.0E0)
        S1 = S1*CSCL
        S2 = S2*CSCL
   40 CONTINUE
      RETURN
   50 CONTINUE
      NZ = -1
      IF(NW.EQ.(-2)) NZ=-2
      RETURN
   60 CONTINUE
      IF (IFORM.EQ.2) GO TO 70
C-----------------------------------------------------------------------
C     ASYMPTOTIC EXPANSION FOR I(FNU,Z) FOR LARGE FNU APPLIED IN
C     -PI/3.LE.ARG(Z).LE.PI/3
C-----------------------------------------------------------------------
      CALL CUNI1(Z, FNU, KODE, N, Y, NW, NLAST, FNUL, TOL, ELIM, ALIM)
      GO TO 80
   70 CONTINUE
C-----------------------------------------------------------------------
C     ASYMPTOTIC EXPANSION FOR J(FNU,Z*EXP(M*HPI)) FOR LARGE FNU
C     APPLIED IN PI/3.LT.ABS(ARG(Z)).LE.PI/2 WHERE M=+I OR -I
C     AND HPI=PI/2
C-----------------------------------------------------------------------
      CALL CUNI2(Z, FNU, KODE, N, Y, NW, NLAST, FNUL, TOL, ELIM, ALIM)
   80 CONTINUE
      IF (NW.LT.0) GO TO 50
      NZ = NW
      RETURN
   90 CONTINUE
      NLAST = N
      RETURN
      END

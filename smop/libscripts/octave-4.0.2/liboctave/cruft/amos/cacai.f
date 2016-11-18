      SUBROUTINE CACAI(Z, FNU, KODE, MR, N, Y, NZ, RL, TOL, ELIM, ALIM)
C***BEGIN PROLOGUE  CACAI
C***REFER TO  CAIRY
C
C     CACAI APPLIES THE ANALYTIC CONTINUATION FORMULA
C
C         K(FNU,ZN*EXP(MP))=K(FNU,ZN)*EXP(-MP*FNU) - MP*I(FNU,ZN)
C                 MP=PI*MR*CMPLX(0.0,1.0)
C
C     TO CONTINUE THE K FUNCTION FROM THE RIGHT HALF TO THE LEFT
C     HALF Z PLANE FOR USE WITH CAIRY WHERE FNU=1/3 OR 2/3 AND N=1.
C     CACAI IS THE SAME AS CACON WITH THE PARTS FOR LARGER ORDERS AND
C     RECURRENCE REMOVED. A RECURSIVE CALL TO CACON CAN RESULT IF CACON
C     IS CALLED FROM CAIRY.
C
C***ROUTINES CALLED  CASYI,CBKNU,CMLRI,CSERI,CS1S2,R1MACH
C***END PROLOGUE  CACAI
      COMPLEX CSGN, CSPN, C1, C2, Y, Z, ZN, CY
      REAL ALIM, ARG, ASCLE, AZ, CPN, DFNU, ELIM, FMR, FNU, PI, RL,
     * SGN, SPN, TOL, YY, R1MACH
      INTEGER INU, IUF, KODE, MR, N, NN, NW, NZ
      DIMENSION Y(N), CY(2)
      DATA PI / 3.14159265358979324E0 /
      NZ = 0
      ZN = -Z
      AZ = CABS(Z)
      NN = N
      DFNU = FNU + FLOAT(N-1)
      IF (AZ.LE.2.0E0) GO TO 10
      IF (AZ*AZ*0.25E0.GT.DFNU+1.0E0) GO TO 20
   10 CONTINUE
C-----------------------------------------------------------------------
C     POWER SERIES FOR THE I FUNCTION
C-----------------------------------------------------------------------
      CALL CSERI(ZN, FNU, KODE, NN, Y, NW, TOL, ELIM, ALIM)
      GO TO 40
   20 CONTINUE
      IF (AZ.LT.RL) GO TO 30
C-----------------------------------------------------------------------
C     ASYMPTOTIC EXPANSION FOR LARGE Z FOR THE I FUNCTION
C-----------------------------------------------------------------------
      CALL CASYI(ZN, FNU, KODE, NN, Y, NW, RL, TOL, ELIM, ALIM)
      IF (NW.LT.0) GO TO 70
      GO TO 40
   30 CONTINUE
C-----------------------------------------------------------------------
C     MILLER ALGORITHM NORMALIZED BY THE SERIES FOR THE I FUNCTION
C-----------------------------------------------------------------------
      CALL CMLRI(ZN, FNU, KODE, NN, Y, NW, TOL)
      IF(NW.LT.0) GO TO 70
   40 CONTINUE
C-----------------------------------------------------------------------
C     ANALYTIC CONTINUATION TO THE LEFT HALF PLANE FOR THE K FUNCTION
C-----------------------------------------------------------------------
      CALL CBKNU(ZN, FNU, KODE, 1, CY, NW, TOL, ELIM, ALIM)
      IF (NW.NE.0) GO TO 70
      FMR = FLOAT(MR)
      SGN = -SIGN(PI,FMR)
      CSGN = CMPLX(0.0E0,SGN)
      IF (KODE.EQ.1) GO TO 50
      YY = -AIMAG(ZN)
      CPN = COS(YY)
      SPN = SIN(YY)
      CSGN = CSGN*CMPLX(CPN,SPN)
   50 CONTINUE
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
      C1 = CY(1)
      C2 = Y(1)
      IF (KODE.EQ.1) GO TO 60
      IUF = 0
      ASCLE = 1.0E+3*R1MACH(1)/TOL
      CALL CS1S2(ZN, C1, C2, NW, ASCLE, ALIM, IUF)
      NZ = NZ + NW
   60 CONTINUE
      Y(1) = CSPN*C1 + CSGN*C2
      RETURN
   70 CONTINUE
      NZ = -1
      IF(NW.EQ.(-2)) NZ=-2
      RETURN
      END

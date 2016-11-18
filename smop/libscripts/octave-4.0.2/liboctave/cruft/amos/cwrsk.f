      SUBROUTINE CWRSK(ZR, FNU, KODE, N, Y, NZ, CW, TOL, ELIM, ALIM)
C***BEGIN PROLOGUE  CWRSK
C***REFER TO  CBESI,CBESK
C
C     CWRSK COMPUTES THE I BESSEL FUNCTION FOR RE(Z).GE.0.0 BY
C     NORMALIZING THE I FUNCTION RATIOS FROM CRATI BY THE WRONSKIAN
C
C***ROUTINES CALLED  CBKNU,CRATI,R1MACH
C***END PROLOGUE  CWRSK
      COMPLEX CINU, CSCL, CT, CW, C1, C2, RCT, ST, Y, ZR
      REAL ACT, ACW, ALIM, ASCLE, ELIM, FNU, S1, S2, TOL, YY
      INTEGER I, KODE, N, NW, NZ
      DIMENSION Y(N), CW(2)
C-----------------------------------------------------------------------
C     I(FNU+I-1,Z) BY BACKWARD RECURRENCE FOR RATIOS
C     Y(I)=I(FNU+I,Z)/I(FNU+I-1,Z) FROM CRATI NORMALIZED BY THE
C     WRONSKIAN WITH K(FNU,Z) AND K(FNU+1,Z) FROM CBKNU.
C-----------------------------------------------------------------------
      NZ = 0
      CALL CBKNU(ZR, FNU, KODE, 2, CW, NW, TOL, ELIM, ALIM)
      IF (NW.NE.0) GO TO 50
      CALL CRATI(ZR, FNU, N, Y, TOL)
C-----------------------------------------------------------------------
C     RECUR FORWARD ON I(FNU+1,Z) = R(FNU,Z)*I(FNU,Z),
C     R(FNU+J-1,Z)=Y(J),  J=1,...,N
C-----------------------------------------------------------------------
      CINU = CMPLX(1.0E0,0.0E0)
      IF (KODE.EQ.1) GO TO 10
      YY = AIMAG(ZR)
      S1 = COS(YY)
      S2 = SIN(YY)
      CINU = CMPLX(S1,S2)
   10 CONTINUE
C-----------------------------------------------------------------------
C     ON LOW EXPONENT MACHINES THE K FUNCTIONS CAN BE CLOSE TO BOTH
C     THE UNDER AND OVERFLOW LIMITS AND THE NORMALIZATION MUST BE
C     SCALED TO PREVENT OVER OR UNDERFLOW. CUOIK HAS DETERMINED THAT
C     THE RESULT IS ON SCALE.
C-----------------------------------------------------------------------
      ACW = CABS(CW(2))
      ASCLE = 1.0E+3*R1MACH(1)/TOL
      CSCL = CMPLX(1.0E0,0.0E0)
      IF (ACW.GT.ASCLE) GO TO 20
      CSCL = CMPLX(1.0E0/TOL,0.0E0)
      GO TO 30
   20 CONTINUE
      ASCLE = 1.0E0/ASCLE
      IF (ACW.LT.ASCLE) GO TO 30
      CSCL = CMPLX(TOL,0.0E0)
   30 CONTINUE
      C1 = CW(1)*CSCL
      C2 = CW(2)*CSCL
      ST = Y(1)
C-----------------------------------------------------------------------
C     CINU=CINU*(CONJG(CT)/CABS(CT))*(1.0E0/CABS(CT) PREVENTS
C     UNDER- OR OVERFLOW PREMATURELY BY SQUARING CABS(CT)
C-----------------------------------------------------------------------
      CT = ZR*(C2+ST*C1)
      ACT = CABS(CT)
      RCT = CMPLX(1.0E0/ACT,0.0E0)
      CT = CONJG(CT)*RCT
      CINU = CINU*RCT*CT
      Y(1) = CINU*CSCL
      IF (N.EQ.1) RETURN
      DO 40 I=2,N
        CINU = ST*CINU
        ST = Y(I)
        Y(I) = CINU*CSCL
   40 CONTINUE
      RETURN
   50 CONTINUE
      NZ = -1
      IF(NW.EQ.(-2)) NZ=-2
      RETURN
      END

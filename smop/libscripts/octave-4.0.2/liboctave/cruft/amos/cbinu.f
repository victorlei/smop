      SUBROUTINE CBINU(Z, FNU, KODE, N, CY, NZ, RL, FNUL, TOL, ELIM,
     * ALIM)
C***BEGIN PROLOGUE  CBINU
C***REFER TO  CBESH,CBESI,CBESJ,CBESK,CAIRY,CBIRY
C
C     CBINU COMPUTES THE I FUNCTION IN THE RIGHT HALF Z PLANE
C
C***ROUTINES CALLED  CASYI,CBUNI,CMLRI,CSERI,CUOIK,CWRSK
C***END PROLOGUE  CBINU
      COMPLEX CW, CY, CZERO, Z
      REAL ALIM, AZ, DFNU, ELIM, FNU, FNUL, RL, TOL
      INTEGER I, INW, KODE, N, NLAST, NN, NUI, NW, NZ
      DIMENSION CY(N), CW(2)
      DATA CZERO / (0.0E0,0.0E0) /
C
      NZ = 0
      AZ = CABS(Z)
      NN = N
      DFNU = FNU + FLOAT(N-1)
      IF (AZ.LE.2.0E0) GO TO 10
      IF (AZ*AZ*0.25E0.GT.DFNU+1.0E0) GO TO 20
   10 CONTINUE
C-----------------------------------------------------------------------
C     POWER SERIES
C-----------------------------------------------------------------------
      CALL CSERI(Z, FNU, KODE, NN, CY, NW, TOL, ELIM, ALIM)
      INW = IABS(NW)
      NZ = NZ + INW
      NN = NN - INW
      IF (NN.EQ.0) RETURN
      IF (NW.GE.0) GO TO 120
      DFNU = FNU + FLOAT(NN-1)
   20 CONTINUE
      IF (AZ.LT.RL) GO TO 40
      IF (DFNU.LE.1.0E0) GO TO 30
      IF (AZ+AZ.LT.DFNU*DFNU) GO TO 50
C-----------------------------------------------------------------------
C     ASYMPTOTIC EXPANSION FOR LARGE Z
C-----------------------------------------------------------------------
   30 CONTINUE
      CALL CASYI(Z, FNU, KODE, NN, CY, NW, RL, TOL, ELIM, ALIM)
      IF (NW.LT.0) GO TO 130
      GO TO 120
   40 CONTINUE
      IF (DFNU.LE.1.0E0) GO TO 70
   50 CONTINUE
C-----------------------------------------------------------------------
C     OVERFLOW AND UNDERFLOW TEST ON I SEQUENCE FOR MILLER ALGORITHM
C-----------------------------------------------------------------------
      CALL CUOIK(Z, FNU, KODE, 1, NN, CY, NW, TOL, ELIM, ALIM)
      IF (NW.LT.0) GO TO 130
      NZ = NZ + NW
      NN = NN - NW
      IF (NN.EQ.0) RETURN
      DFNU = FNU+FLOAT(NN-1)
      IF (DFNU.GT.FNUL) GO TO 110
      IF (AZ.GT.FNUL) GO TO 110
   60 CONTINUE
      IF (AZ.GT.RL) GO TO 80
   70 CONTINUE
C-----------------------------------------------------------------------
C     MILLER ALGORITHM NORMALIZED BY THE SERIES
C-----------------------------------------------------------------------
      CALL CMLRI(Z, FNU, KODE, NN, CY, NW, TOL)
      IF(NW.LT.0) GO TO 130
      GO TO 120
   80 CONTINUE
C-----------------------------------------------------------------------
C     MILLER ALGORITHM NORMALIZED BY THE WRONSKIAN
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C     OVERFLOW TEST ON K FUNCTIONS USED IN WRONSKIAN
C-----------------------------------------------------------------------
      CALL CUOIK(Z, FNU, KODE, 2, 2, CW, NW, TOL, ELIM, ALIM)
      IF (NW.GE.0) GO TO 100
      NZ = NN
      DO 90 I=1,NN
        CY(I) = CZERO
   90 CONTINUE
      RETURN
  100 CONTINUE
      IF (NW.GT.0) GO TO 130
      CALL CWRSK(Z, FNU, KODE, NN, CY, NW, CW, TOL, ELIM, ALIM)
      IF (NW.LT.0) GO TO 130
      GO TO 120
  110 CONTINUE
C-----------------------------------------------------------------------
C     INCREMENT FNU+NN-1 UP TO FNUL, COMPUTE AND RECUR BACKWARD
C-----------------------------------------------------------------------
      NUI = INT(FNUL-DFNU) + 1
      NUI = MAX0(NUI,0)
      CALL CBUNI(Z, FNU, KODE, NN, CY, NW, NUI, NLAST, FNUL, TOL, ELIM,
     * ALIM)
      IF (NW.LT.0) GO TO 130
      NZ = NZ + NW
      IF (NLAST.EQ.0) GO TO 120
      NN = NLAST
      GO TO 60
  120 CONTINUE
      RETURN
  130 CONTINUE
      NZ = -1
      IF(NW.EQ.(-2)) NZ=-2
      RETURN
      END

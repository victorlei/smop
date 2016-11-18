      DOUBLE PRECISION FUNCTION XZABS(ZR, ZI)
C***BEGIN PROLOGUE  XZABS
C***REFER TO  ZBESH,ZBESI,ZBESJ,ZBESK,ZBESY,ZAIRY,ZBIRY
C
C     XZABS COMPUTES THE ABSOLUTE VALUE OR MAGNITUDE OF A DOUBLE
C     PRECISION COMPLEX VARIABLE CMPLX(ZR,ZI)
C
C***ROUTINES CALLED  (NONE)
C***END PROLOGUE  XZABS
      DOUBLE PRECISION ZR, ZI, U, V, Q, S
      U = DABS(ZR)
      V = DABS(ZI)
      S = U + V
C-----------------------------------------------------------------------
C     S*1.0D0 MAKES AN UNNORMALIZED UNDERFLOW ON CDC MACHINES INTO A
C     TRUE FLOATING ZERO
C-----------------------------------------------------------------------
      S = S*1.0D+0
      IF (S.EQ.0.0D+0) GO TO 20
      IF (U.GT.V) GO TO 10
      Q = U/V
      XZABS = V*DSQRT(1.D+0+Q*Q)
      RETURN
   10 Q = V/U
      XZABS = U*DSQRT(1.D+0+Q*Q)
      RETURN
   20 XZABS = 0.0D+0
      RETURN
      END

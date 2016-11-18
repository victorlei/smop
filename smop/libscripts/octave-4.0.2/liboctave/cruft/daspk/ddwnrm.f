C Work performed under the auspices of the U.S. Department of Energy
C by Lawrence Livermore National Laboratory under contract number
C W-7405-Eng-48.
C
      DOUBLE PRECISION FUNCTION DDWNRM(NEQ,V,RWT,RPAR,IPAR)
C
C***BEGIN PROLOGUE  DDWNRM
C***ROUTINES CALLED  (NONE)
C***DATE WRITTEN   890101   (YYMMDD)
C***REVISION DATE  900926   (YYMMDD)
C***END PROLOGUE  DDWNRM
C-----------------------------------------------------------------------
C     This function routine computes the weighted
C     root-mean-square norm of the vector of length
C     NEQ contained in the array V, with reciprocal weights
C     contained in the array RWT of length NEQ.
C        DDWNRM=SQRT((1/NEQ)*SUM(V(I)*RWT(I))**2)
C-----------------------------------------------------------------------
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      DIMENSION V(*),RWT(*)
      DIMENSION RPAR(*),IPAR(*)
      DDWNRM = 0.0D0
      VMAX = 0.0D0
      DO 10 I = 1,NEQ
        IF(ABS(V(I)*RWT(I)) .GT. VMAX) VMAX = ABS(V(I)*RWT(I))
10    CONTINUE
      IF(VMAX .LE. 0.0D0) GO TO 30
      SUM = 0.0D0
      DO 20 I = 1,NEQ
20      SUM = SUM + ((V(I)*RWT(I))/VMAX)**2
      DDWNRM = VMAX*SQRT(SUM/NEQ)
30    CONTINUE
      RETURN
C
C------END OF FUNCTION DDWNRM-------------------------------------------
      END

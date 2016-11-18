C Work performed under the auspices of the U.S. Department of Energy
C by Lawrence Livermore National Laboratory under contract number
C W-7405-Eng-48.
C
      SUBROUTINE DINVWT(NEQ,WT,IER)
C
C***BEGIN PROLOGUE  DINVWT
C***REFER TO  DDASPK
C***ROUTINES CALLED  (NONE)
C***DATE WRITTEN   950125   (YYMMDD)
C***END PROLOGUE  DINVWT
C-----------------------------------------------------------------------
C     This subroutine checks the error weight vector WT, of length NEQ,
C     for components that are .le. 0, and if none are found, it
C     inverts the WT(I) in place.  This replaces division operations
C     with multiplications in all norm evaluations.
C     IER is returned as 0 if all WT(I) were found positive,
C     and the first I with WT(I) .le. 0.0 otherwise.
C-----------------------------------------------------------------------
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      DIMENSION WT(*)
C
      DO 10 I = 1,NEQ
        IF (WT(I) .LE. 0.0D0) GO TO 30
 10     CONTINUE
      DO 20 I = 1,NEQ
 20     WT(I) = 1.0D0/WT(I)
      IER = 0
      RETURN
C
 30   IER = I
      RETURN
C
C------END OF SUBROUTINE DINVWT-----------------------------------------
      END

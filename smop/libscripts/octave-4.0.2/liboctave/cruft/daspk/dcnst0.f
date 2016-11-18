C Work performed under the auspices of the U.S. Department of Energy
C by Lawrence Livermore National Laboratory under contract number
C W-7405-Eng-48.
C
      SUBROUTINE DCNST0 (NEQ, Y, ICNSTR, IRET)
C
C***BEGIN PROLOGUE  DCNST0
C***DATE WRITTEN   950808   (YYMMDD)
C***REVISION DATE  950808   (YYMMDD)
C
C
C-----------------------------------------------------------------------
C***DESCRIPTION
C
C This subroutine checks for constraint violations in the initial
C approximate solution u.
C
C On entry
C
C   NEQ    -- size of the nonlinear system, and the length of arrays
C             Y and ICNSTR.
C
C   Y      -- real array containing the initial approximate root.
C
C   ICNSTR -- INTEGER array of length NEQ containing flags indicating
C             which entries in Y are to be constrained.
C             if ICNSTR(I) =  2, then Y(I) must be .GT. 0,
C             if ICNSTR(I) =  1, then Y(I) must be .GE. 0,
C             if ICNSTR(I) = -1, then Y(I) must be .LE. 0, while
C             if ICNSTR(I) = -2, then Y(I) must be .LT. 0, while
C             if ICNSTR(I) =  0, then Y(I) is not constrained.
C
C On return
C
C   IRET   -- output flag.
C             IRET=0    means that u satisfied all constraints.
C             IRET.NE.0 means that Y(IRET) failed to satisfy its
C                       constraint.
C
C-----------------------------------------------------------------------
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      DIMENSION Y(NEQ), ICNSTR(NEQ)
      SAVE ZERO
      DATA ZERO/0.D0/
C-----------------------------------------------------------------------
C Check constraints for initial Y.  If a constraint has been violated,
C set IRET = I to signal an error return to calling routine.
C-----------------------------------------------------------------------
      IRET = 0
      DO 100 I = 1,NEQ
         IF (ICNSTR(I) .EQ. 2) THEN
            IF (Y(I) .LE. ZERO) THEN
               IRET = I
               RETURN
            ENDIF
         ELSEIF (ICNSTR(I) .EQ. 1) THEN
            IF (Y(I) .LT. ZERO) THEN
               IRET = I
               RETURN
            ENDIF
         ELSEIF (ICNSTR(I) .EQ. -1) THEN
            IF (Y(I) .GT. ZERO) THEN
               IRET = I
               RETURN
            ENDIF
         ELSEIF (ICNSTR(I) .EQ. -2) THEN
            IF (Y(I) .GE. ZERO) THEN
               IRET = I
               RETURN
            ENDIF
        ENDIF
 100  CONTINUE
      RETURN
C----------------------- END OF SUBROUTINE DCNST0 ----------------------
      END

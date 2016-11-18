C Work performed under the auspices of the U.S. Department of Energy
C by Lawrence Livermore National Laboratory under contract number
C W-7405-Eng-48.
C
      SUBROUTINE DCNSTR (NEQ, Y, YNEW, ICNSTR, TAU, RLX, IRET, IVAR)
C
C***BEGIN PROLOGUE  DCNSTR
C***DATE WRITTEN   950808   (YYMMDD)
C***REVISION DATE  950814   (YYMMDD)
C
C
C-----------------------------------------------------------------------
C***DESCRIPTION
C
C This subroutine checks for constraint violations in the proposed
C new approximate solution YNEW.
C If a constraint violation occurs, then a new step length, TAU,
C is calculated, and this value is to be given to the linesearch routine
C to calculate a new approximate solution YNEW.
C
C On entry:
C
C   NEQ    -- size of the nonlinear system, and the length of arrays
C             Y, YNEW and ICNSTR.
C
C   Y      -- real array containing the current approximate y.
C
C   YNEW   -- real array containing the new approximate y.
C
C   ICNSTR -- INTEGER array of length NEQ containing flags indicating
C             which entries in YNEW are to be constrained.
C             if ICNSTR(I) =  2, then YNEW(I) must be .GT. 0,
C             if ICNSTR(I) =  1, then YNEW(I) must be .GE. 0,
C             if ICNSTR(I) = -1, then YNEW(I) must be .LE. 0, while
C             if ICNSTR(I) = -2, then YNEW(I) must be .LT. 0, while
C             if ICNSTR(I) =  0, then YNEW(I) is not constrained.
C
C   RLX    -- real scalar restricting update, if ICNSTR(I) = 2 or -2,
C             to ABS( (YNEW-Y)/Y ) < FAC2*RLX in component I.
C
C   TAU    -- the current size of the step length for the linesearch.
C
C On return
C
C   TAU    -- the adjusted size of the step length if a constraint
C             violation occurred (otherwise, it is unchanged).  it is
C             the step length to give to the linesearch routine.
C
C   IRET   -- output flag.
C             IRET=0 means that YNEW satisfied all constraints.
C             IRET=1 means that YNEW failed to satisfy all the
C                    constraints, and a new linesearch step
C                    must be computed.
C
C   IVAR   -- index of variable causing constraint to be violated.
C
C-----------------------------------------------------------------------
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      DIMENSION Y(NEQ), YNEW(NEQ), ICNSTR(NEQ)
      SAVE FAC, FAC2, ZERO
      DATA FAC /0.6D0/, FAC2 /0.9D0/, ZERO/0.0D0/
C-----------------------------------------------------------------------
C Check constraints for proposed new step YNEW.  If a constraint has
C been violated, then calculate a new step length, TAU, to be
C used in the linesearch routine.
C-----------------------------------------------------------------------
      IRET = 0
      RDYMX = ZERO
      IVAR = 0
      DO 100 I = 1,NEQ
C
         IF (ICNSTR(I) .EQ. 2) THEN
            RDY = ABS( (YNEW(I)-Y(I))/Y(I) )
            IF (RDY .GT. RDYMX) THEN
               RDYMX = RDY
               IVAR = I
            ENDIF
            IF (YNEW(I) .LE. ZERO) THEN
               TAU = FAC*TAU
               IVAR = I
               IRET = 1
               RETURN
            ENDIF
C
         ELSEIF (ICNSTR(I) .EQ. 1) THEN
            IF (YNEW(I) .LT. ZERO) THEN
               TAU = FAC*TAU
               IVAR = I
               IRET = 1
               RETURN
            ENDIF
C
         ELSEIF (ICNSTR(I) .EQ. -1) THEN
            IF (YNEW(I) .GT. ZERO) THEN
               TAU = FAC*TAU
               IVAR = I
               IRET = 1
               RETURN
            ENDIF
C
         ELSEIF (ICNSTR(I) .EQ. -2) THEN
            RDY = ABS( (YNEW(I)-Y(I))/Y(I) )
            IF (RDY .GT. RDYMX) THEN
               RDYMX = RDY
               IVAR = I
            ENDIF
            IF (YNEW(I) .GE. ZERO) THEN
               TAU = FAC*TAU
               IVAR = I
               IRET = 1
               RETURN
            ENDIF
C
         ENDIF
 100  CONTINUE

      IF(RDYMX .GE. RLX) THEN
         TAU = FAC2*TAU*RLX/RDYMX
         IRET = 1
      ENDIF
C
      RETURN
C----------------------- END OF SUBROUTINE DCNSTR ----------------------
      END

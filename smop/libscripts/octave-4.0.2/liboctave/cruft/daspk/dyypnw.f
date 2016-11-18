C Work performed under the auspices of the U.S. Department of Energy
C by Lawrence Livermore National Laboratory under contract number
C W-7405-Eng-48.
C
      SUBROUTINE DYYPNW (NEQ, Y, YPRIME, CJ, RL, P, ICOPT, ID,
     *                   YNEW, YPNEW)
C
C***BEGIN PROLOGUE  DYYPNW
C***REFER TO  DLINSK
C***DATE WRITTEN   940830   (YYMMDD)
C
C
C-----------------------------------------------------------------------
C***DESCRIPTION
C
C     DYYPNW calculates the new (Y,YPRIME) pair needed in the
C     linesearch algorithm based on the current lambda value.  It is
C     called by DLINSK and DLINSD.  Based on the ICOPT and ID values,
C     the corresponding entry in Y or YPRIME is updated.
C
C     In addition to the parameters described in the calling programs,
C     the parameters represent
C
C     P      -- Array of length NEQ that contains the current
C               approximate Newton step.
C     RL     -- Scalar containing the current lambda value.
C     YNEW   -- Array of length NEQ containing the updated Y vector.
C     YPNEW  -- Array of length NEQ containing the updated YPRIME
C               vector.
C-----------------------------------------------------------------------
C
C***ROUTINES CALLED (NONE)
C
C***END PROLOGUE  DYYPNW
C
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION Y(*), YPRIME(*), YNEW(*), YPNEW(*), ID(*), P(*)
C
      IF (ICOPT .EQ. 1) THEN
         DO 10 I=1,NEQ
            IF(ID(I) .LT. 0) THEN
               YNEW(I) = Y(I) - RL*P(I)
               YPNEW(I) = YPRIME(I)
            ELSE
               YNEW(I) = Y(I)
               YPNEW(I) = YPRIME(I) - RL*CJ*P(I)
            ENDIF
 10      CONTINUE
      ELSE
         DO 20 I = 1,NEQ
            YNEW(I) = Y(I) - RL*P(I)
            YPNEW(I) = YPRIME(I)
 20      CONTINUE
      ENDIF
      RETURN
C----------------------- END OF SUBROUTINE DYYPNW ----------------------
      END

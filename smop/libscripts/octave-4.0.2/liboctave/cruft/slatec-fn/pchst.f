*DECK PCHST
      REAL FUNCTION PCHST (ARG1, ARG2)
C***BEGIN PROLOGUE  PCHST
C***SUBSIDIARY
C***PURPOSE  PCHIP Sign-Testing Routine
C***LIBRARY   SLATEC (PCHIP)
C***TYPE      SINGLE PRECISION (PCHST-S, DPCHST-D)
C***AUTHOR  Fritsch, F. N., (LLNL)
C***DESCRIPTION
C
C         PCHST:  PCHIP Sign-Testing Routine.
C
C     Returns:
C        -1. if ARG1 and ARG2 are of opposite sign.
C         0. if either argument is zero.
C        +1. if ARG1 and ARG2 are of the same sign.
C
C     The object is to do this without multiplying ARG1*ARG2, to avoid
C     possible over/underflow problems.
C
C  Fortran intrinsics used:  SIGN.
C
C***SEE ALSO  PCHCE, PCHCI, PCHCS, PCHIM
C***ROUTINES CALLED  (NONE)
C***REVISION HISTORY  (YYMMDD)
C   811103  DATE WRITTEN
C   820805  Converted to SLATEC library version.
C   870813  Minor cosmetic changes.
C   890411  Added SAVE statements (Vers. 3.2).
C   890411  REVISION DATE from Version 3.2
C   891214  Prologue converted to Version 4.0 format.  (BAB)
C   900328  Added TYPE section.  (WRB)
C   910408  Updated AUTHOR and DATE WRITTEN sections in prologue.  (WRB)
C   930503  Improved purpose.  (FNF)
C***END PROLOGUE  PCHST
C
C**End
C
C  DECLARE ARGUMENTS.
C
      REAL  ARG1, ARG2
C
C  DECLARE LOCAL VARIABLES.
C
      REAL  ONE, ZERO
      SAVE ZERO, ONE
      DATA  ZERO /0./,  ONE /1./
C
C  PERFORM THE TEST.
C
C***FIRST EXECUTABLE STATEMENT  PCHST
      PCHST = SIGN(ONE,ARG1) * SIGN(ONE,ARG2)
      IF ((ARG1.EQ.ZERO) .OR. (ARG2.EQ.ZERO))  PCHST = ZERO
C
      RETURN
C------------- LAST LINE OF PCHST FOLLOWS ------------------------------
      END

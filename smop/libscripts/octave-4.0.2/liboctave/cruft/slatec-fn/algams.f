*DECK ALGAMS
      SUBROUTINE ALGAMS (X, ALGAM, SGNGAM)
C***BEGIN PROLOGUE  ALGAMS
C***PURPOSE  Compute the logarithm of the absolute value of the Gamma
C            function.
C***LIBRARY   SLATEC (FNLIB)
C***CATEGORY  C7A
C***TYPE      SINGLE PRECISION (ALGAMS-S, DLGAMS-D)
C***KEYWORDS  ABSOLUTE VALUE OF THE LOGARITHM OF THE GAMMA FUNCTION,
C             FNLIB, SPECIAL FUNCTIONS
C***AUTHOR  Fullerton, W., (LANL)
C***DESCRIPTION
C
C Evaluates the logarithm of the absolute value of the gamma
C function.
C     X           - input argument
C     ALGAM       - result
C     SGNGAM      - is set to the sign of GAMMA(X) and will
C                   be returned at +1.0 or -1.0.
C
C***REFERENCES  (NONE)
C***ROUTINES CALLED  ALNGAM
C***REVISION HISTORY  (YYMMDD)
C   770701  DATE WRITTEN
C   890531  Changed all specific intrinsics to generic.  (WRB)
C   890531  REVISION DATE from Version 3.2
C   891214  Prologue converted to Version 4.0 format.  (BAB)
C***END PROLOGUE  ALGAMS
C***FIRST EXECUTABLE STATEMENT  ALGAMS
      ALGAM = ALNGAM(X)
      SGNGAM = 1.0
      IF (X.GT.0.0) RETURN
C
      INT = MOD (-AINT(X), 2.0) + 0.1
      IF (INT.EQ.0) SGNGAM = -1.0
C
      RETURN
      END

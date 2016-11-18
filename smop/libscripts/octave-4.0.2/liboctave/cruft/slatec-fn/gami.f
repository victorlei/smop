*DECK GAMI
      FUNCTION GAMI (A, X)
C***BEGIN PROLOGUE  GAMI
C***PURPOSE  Evaluate the incomplete Gamma function.
C***LIBRARY   SLATEC (FNLIB)
C***CATEGORY  C7E
C***TYPE      SINGLE PRECISION (GAMI-S, DGAMI-D)
C***KEYWORDS  FNLIB, INCOMPLETE GAMMA FUNCTION, SPECIAL FUNCTIONS
C***AUTHOR  Fullerton, W., (LANL)
C***DESCRIPTION
C
C Evaluate the incomplete gamma function defined by
C
C GAMI = integral from T = 0 to X of EXP(-T) * T**(A-1.0) .
C
C GAMI is evaluated for positive values of A and non-negative values
C of X.  A slight deterioration of 2 or 3 digits accuracy will occur
C when GAMI is very large or very small, because logarithmic variables
C are used.  GAMI, A, and X are single precision.
C
C***REFERENCES  (NONE)
C***ROUTINES CALLED  ALNGAM, GAMIT, XERMSG
C***REVISION HISTORY  (YYMMDD)
C   770701  DATE WRITTEN
C   890531  Changed all specific intrinsics to generic.  (WRB)
C   890531  REVISION DATE from Version 3.2
C   891214  Prologue converted to Version 4.0 format.  (BAB)
C   900315  CALLs to XERROR changed to CALLs to XERMSG.  (THJ)
C***END PROLOGUE  GAMI
C***FIRST EXECUTABLE STATEMENT  GAMI
      IF (A .LE. 0.0) CALL XERMSG ('SLATEC', 'GAMI',
     +   'A MUST BE GT ZERO', 1, 2)
      IF (X .LT. 0.0) CALL XERMSG ('SLATEC', 'GAMI',
     +   'X MUST BE GE ZERO', 2, 2)
C
      GAMI = 0.0
      IF (X.EQ.0.0) RETURN
C
C THE ONLY ERROR POSSIBLE IN THE EXPRESSION BELOW IS A FATAL OVERFLOW.
      FACTOR = EXP (ALNGAM(A) + A*LOG(X) )
C
      GAMI = FACTOR * GAMIT(A, X)
C
      RETURN
      END

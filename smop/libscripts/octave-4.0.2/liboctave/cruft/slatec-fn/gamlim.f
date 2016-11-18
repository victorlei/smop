*DECK GAMLIM
      SUBROUTINE GAMLIM (XMIN, XMAX)
C***BEGIN PROLOGUE  GAMLIM
C***PURPOSE  Compute the minimum and maximum bounds for the argument in
C            the Gamma function.
C***LIBRARY   SLATEC (FNLIB)
C***CATEGORY  C7A, R2
C***TYPE      SINGLE PRECISION (GAMLIM-S, DGAMLM-D)
C***KEYWORDS  COMPLETE GAMMA FUNCTION, FNLIB, LIMITS, SPECIAL FUNCTIONS
C***AUTHOR  Fullerton, W., (LANL)
C***DESCRIPTION
C
C Calculate the minimum and maximum legal bounds for X in GAMMA(X).
C XMIN and XMAX are not the only bounds, but they are the only non-
C trivial ones to calculate.
C
C             Output Arguments --
C XMIN   minimum legal value of X in GAMMA(X).  Any smaller value of
C        X might result in underflow.
C XMAX   maximum legal value of X in GAMMA(X).  Any larger value will
C        cause overflow.
C
C***REFERENCES  (NONE)
C***ROUTINES CALLED  R1MACH, XERMSG
C***REVISION HISTORY  (YYMMDD)
C   770401  DATE WRITTEN
C   890531  Changed all specific intrinsics to generic.  (WRB)
C   890531  REVISION DATE from Version 3.2
C   891214  Prologue converted to Version 4.0 format.  (BAB)
C   900315  CALLs to XERROR changed to CALLs to XERMSG.  (THJ)
C***END PROLOGUE  GAMLIM
C***FIRST EXECUTABLE STATEMENT  GAMLIM
      ALNSML = LOG(R1MACH(1))
      XMIN = -ALNSML
      DO 10 I=1,10
        XOLD = XMIN
        XLN = LOG(XMIN)
        XMIN = XMIN - XMIN*((XMIN+0.5)*XLN - XMIN - 0.2258 + ALNSML)
     1    / (XMIN*XLN + 0.5)
        IF (ABS(XMIN-XOLD).LT.0.005) GO TO 20
 10   CONTINUE
      CALL XERMSG ('SLATEC', 'GAMLIM', 'UNABLE TO FIND XMIN', 1, 2)
C
 20   XMIN = -XMIN + 0.01
C
      ALNBIG = LOG(R1MACH(2))
      XMAX = ALNBIG
      DO 30 I=1,10
        XOLD = XMAX
        XLN = LOG(XMAX)
        XMAX = XMAX - XMAX*((XMAX-0.5)*XLN - XMAX + 0.9189 - ALNBIG)
     1    / (XMAX*XLN - 0.5)
        IF (ABS(XMAX-XOLD).LT.0.005) GO TO 40
 30   CONTINUE
      CALL XERMSG ('SLATEC', 'GAMLIM', 'UNABLE TO FIND XMAX', 2, 2)
C
 40   XMAX = XMAX - 0.01
      XMIN = MAX (XMIN, -XMAX+1.)
C
      RETURN
      END

*DECK R9LGIC
      FUNCTION R9LGIC (A, X, ALX)
C***BEGIN PROLOGUE  R9LGIC
C***SUBSIDIARY
C***PURPOSE  Compute the log complementary incomplete Gamma function
C            for large X and for A .LE. X.
C***LIBRARY   SLATEC (FNLIB)
C***CATEGORY  C7E
C***TYPE      SINGLE PRECISION (R9LGIC-S, D9LGIC-D)
C***KEYWORDS  COMPLEMENTARY INCOMPLETE GAMMA FUNCTION, FNLIB, LARGE X,
C             LOGARITHM, SPECIAL FUNCTIONS
C***AUTHOR  Fullerton, W., (LANL)
C***DESCRIPTION
C
C Compute the log complementary incomplete gamma function for large X
C and for A .LE. X.
C
C***REFERENCES  (NONE)
C***ROUTINES CALLED  R1MACH, XERMSG
C***REVISION HISTORY  (YYMMDD)
C   770701  DATE WRITTEN
C   890531  Changed all specific intrinsics to generic.  (WRB)
C   890531  REVISION DATE from Version 3.2
C   891214  Prologue converted to Version 4.0 format.  (BAB)
C   900315  CALLs to XERROR changed to CALLs to XERMSG.  (THJ)
C   900720  Routine changed from user-callable to subsidiary.  (WRB)
C***END PROLOGUE  R9LGIC
      SAVE EPS
      DATA EPS / 0.0 /
C***FIRST EXECUTABLE STATEMENT  R9LGIC
      IF (EPS.EQ.0.0) EPS = 0.5*R1MACH(3)
C
      XPA = X + 1.0 - A
      XMA = X - 1.0 - A
C
      R = 0.0
      P = 1.0
      S = P
      DO 10 K=1,200
        FK = K
        T = FK*(A-FK)*(1.0+R)
        R = -T/((XMA+2.0*FK)*(XPA+2.0*FK)+T)
        P = R*P
        S = S + P
        IF (ABS(P).LT.EPS*S) GO TO 20
 10   CONTINUE
      CALL XERMSG ('SLATEC', 'R9LGIC',
     +   'NO CONVERGENCE IN 200 TERMS OF CONTINUED FRACTION', 1, 2)
C
 20   R9LGIC = A*ALX - X + LOG(S/XPA)
C
      RETURN
      END

*DECK R9LGIT
      FUNCTION R9LGIT (A, X, ALGAP1)
C***BEGIN PROLOGUE  R9LGIT
C***SUBSIDIARY
C***PURPOSE  Compute the logarithm of Tricomi's incomplete Gamma
C            function with Perron's continued fraction for large X and
C            A .GE. X.
C***LIBRARY   SLATEC (FNLIB)
C***CATEGORY  C7E
C***TYPE      SINGLE PRECISION (R9LGIT-S, D9LGIT-D)
C***KEYWORDS  FNLIB, INCOMPLETE GAMMA FUNCTION, LOGARITHM,
C             PERRON'S CONTINUED FRACTION, SPECIAL FUNCTIONS, TRICOMI
C***AUTHOR  Fullerton, W., (LANL)
C***DESCRIPTION
C
C Compute the log of Tricomi's incomplete gamma function with Perron's
C continued fraction for large X and for A .GE. X.
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
C***END PROLOGUE  R9LGIT
      SAVE EPS, SQEPS
      DATA EPS, SQEPS / 2*0.0 /
C***FIRST EXECUTABLE STATEMENT  R9LGIT
      IF (EPS.EQ.0.0) EPS = 0.5*R1MACH(3)
      IF (SQEPS.EQ.0.0) SQEPS = SQRT(R1MACH(4))
C
      IF (X .LE. 0.0 .OR. A .LT. X) CALL XERMSG ('SLATEC', 'R9LGIT',
     +   'X SHOULD BE GT 0.0 AND LE A', 2, 2)
C
      AX = A + X
      A1X = AX + 1.0
      R = 0.0
      P = 1.0
      S = P
      DO 20 K=1,200
        FK = K
        T = (A+FK)*X*(1.0+R)
        R = T/((AX+FK)*(A1X+FK)-T)
        P = R*P
        S = S + P
        IF (ABS(P).LT.EPS*S) GO TO 30
 20   CONTINUE
      CALL XERMSG ('SLATEC', 'R9LGIT',
     +   'NO CONVERGENCE IN 200 TERMS OF CONTINUED FRACTION', 3, 2)
C
 30   HSTAR = 1.0 - X*S/A1X
      IF (HSTAR .LT. SQEPS) CALL XERMSG ('SLATEC', 'R9LGIT',
     +   'RESULT LESS THAN HALF PRECISION', 1, 1)
C
      R9LGIT = -X - ALGAP1 - LOG(HSTAR)
C
      RETURN
      END

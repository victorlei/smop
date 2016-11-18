*DECK GAMIT
      REAL FUNCTION GAMIT (A, X)
C***BEGIN PROLOGUE  GAMIT
C***PURPOSE  Calculate Tricomi's form of the incomplete Gamma function.
C***LIBRARY   SLATEC (FNLIB)
C***CATEGORY  C7E
C***TYPE      SINGLE PRECISION (GAMIT-S, DGAMIT-D)
C***KEYWORDS  COMPLEMENTARY INCOMPLETE GAMMA FUNCTION, FNLIB,
C             SPECIAL FUNCTIONS, TRICOMI
C***AUTHOR  Fullerton, W., (LANL)
C***DESCRIPTION
C
C   Evaluate Tricomi's incomplete gamma function defined by
C
C   GAMIT = X**(-A)/GAMMA(A) * integral from 0 to X of EXP(-T) *
C             T**(A-1.)
C
C   for A .GT. 0.0 and by analytic continuation for A .LE. 0.0.
C   GAMMA(X) is the complete gamma function of X.
C
C   GAMIT is evaluated for arbitrary real values of A and for non-
C   negative values of X (even though GAMIT is defined for X .LT.
C   0.0), except that for X = 0 and A .LE. 0.0, GAMIT is infinite,
C   which is a fatal error.
C
C   The function and both arguments are REAL.
C
C   A slight deterioration of 2 or 3 digits accuracy will occur when
C   GAMIT is very large or very small in absolute value, because log-
C   arithmic variables are used.  Also, if the parameter  A  is very
C   close to a negative integer (but not a negative integer), there is
C   a loss of accuracy, which is reported if the result is less than
C   half machine precision.
C
C***REFERENCES  W. Gautschi, A computational procedure for incomplete
C                 gamma functions, ACM Transactions on Mathematical
C                 Software 5, 4 (December 1979), pp. 466-481.
C               W. Gautschi, Incomplete gamma functions, Algorithm 542,
C                 ACM Transactions on Mathematical Software 5, 4
C                 (December 1979), pp. 482-489.
C***ROUTINES CALLED  ALGAMS, ALNGAM, GAMR, R1MACH, R9GMIT, R9LGIC,
C                    R9LGIT, XERCLR, XERMSG
C***REVISION HISTORY  (YYMMDD)
C   770701  DATE WRITTEN
C   890531  Changed all specific intrinsics to generic.  (WRB)
C   890531  REVISION DATE from Version 3.2
C   891214  Prologue converted to Version 4.0 format.  (BAB)
C   900315  CALLs to XERROR changed to CALLs to XERMSG.  (THJ)
C   920528  DESCRIPTION and REFERENCES sections revised.  (WRB)
C***END PROLOGUE  GAMIT
      LOGICAL FIRST
      SAVE ALNEPS, SQEPS, BOT, FIRST
      DATA FIRST /.TRUE./
C***FIRST EXECUTABLE STATEMENT  GAMIT
      IF (FIRST) THEN
         ALNEPS = -LOG(R1MACH(3))
         SQEPS = SQRT(R1MACH(4))
         BOT = LOG(R1MACH(1))
      ENDIF
      FIRST = .FALSE.
C
      IF (X .LT. 0.0) CALL XERMSG ('SLATEC', 'GAMIT', 'X IS NEGATIVE',
     +   2, 2)
C
      IF (X.NE.0.0) ALX = LOG(X)
      SGA = 1.0
      IF (A.NE.0.0) SGA = SIGN (1.0, A)
      AINTA = AINT (A+0.5*SGA)
      AEPS = A - AINTA
C
      IF (X.GT.0.0) GO TO 20
      GAMIT = 0.0
      IF (AINTA.GT.0.0 .OR. AEPS.NE.0.0) GAMIT = GAMR(A+1.0)
      RETURN
C
 20   IF (X.GT.1.0) GO TO 40
      IF (A.GE.(-0.5) .OR. AEPS.NE.0.0) CALL ALGAMS (A+1.0, ALGAP1,
     1  SGNGAM)
      GAMIT = R9GMIT (A, X, ALGAP1, SGNGAM, ALX)
      RETURN
C
 40   IF (A.LT.X) GO TO 50
      T = R9LGIT (A, X, ALNGAM(A+1.0))
      IF (T.LT.BOT) CALL XERCLR
      GAMIT = EXP(T)
      RETURN
C
 50   ALNG = R9LGIC (A, X, ALX)
C
C EVALUATE GAMIT IN TERMS OF LOG(GAMIC(A,X))
C
      H = 1.0
      IF (AEPS.EQ.0.0 .AND. AINTA.LE.0.0) GO TO 60
      CALL ALGAMS (A+1.0, ALGAP1, SGNGAM)
      T = LOG(ABS(A)) + ALNG - ALGAP1
      IF (T.GT.ALNEPS) GO TO 70
      IF (T.GT.(-ALNEPS)) H = 1.0 - SGA*SGNGAM*EXP(T)
      IF (ABS(H).GT.SQEPS) GO TO 60
      CALL XERCLR
      CALL XERMSG ('SLATEC', 'GAMIT', 'RESULT LT HALF PRECISION', 1, 1)
C
 60   T = -A*ALX + LOG(ABS(H))
      IF (T.LT.BOT) CALL XERCLR
      GAMIT = SIGN (EXP(T), H)
      RETURN
C
 70   T = T - A*ALX
      IF (T.LT.BOT) CALL XERCLR
      GAMIT = -SGA*SGNGAM*EXP(T)
      RETURN
C
      END

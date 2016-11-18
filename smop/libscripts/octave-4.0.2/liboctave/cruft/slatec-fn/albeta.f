*DECK ALBETA
      FUNCTION ALBETA (A, B)
C***BEGIN PROLOGUE  ALBETA
C***PURPOSE  Compute the natural logarithm of the complete Beta
C            function.
C***LIBRARY   SLATEC (FNLIB)
C***CATEGORY  C7B
C***TYPE      SINGLE PRECISION (ALBETA-S, DLBETA-D, CLBETA-C)
C***KEYWORDS  FNLIB, LOGARITHM OF THE COMPLETE BETA FUNCTION,
C             SPECIAL FUNCTIONS
C***AUTHOR  Fullerton, W., (LANL)
C***DESCRIPTION
C
C ALBETA computes the natural log of the complete beta function.
C
C Input Parameters:
C       A   real and positive
C       B   real and positive
C
C***REFERENCES  (NONE)
C***ROUTINES CALLED  ALNGAM, ALNREL, GAMMA, R9LGMC, XERMSG
C***REVISION HISTORY  (YYMMDD)
C   770701  DATE WRITTEN
C   890531  Changed all specific intrinsics to generic.  (WRB)
C   890531  REVISION DATE from Version 3.2
C   891214  Prologue converted to Version 4.0 format.  (BAB)
C   900315  CALLs to XERROR changed to CALLs to XERMSG.  (THJ)
C   900326  Removed duplicate information from DESCRIPTION section.
C           (WRB)
C   900727  Added EXTERNAL statement.  (WRB)
C***END PROLOGUE  ALBETA
      EXTERNAL GAMMA
      SAVE SQ2PIL
      DATA SQ2PIL / 0.9189385332 0467274 E0 /
C***FIRST EXECUTABLE STATEMENT  ALBETA
      P = MIN (A, B)
      Q = MAX (A, B)
C
      IF (P .LE. 0.0) CALL XERMSG ('SLATEC', 'ALBETA',
     +   'BOTH ARGUMENTS MUST BE GT ZERO', 1, 2)
      IF (P.GE.10.0) GO TO 30
      IF (Q.GE.10.0) GO TO 20
C
C P AND Q ARE SMALL.
C
      ALBETA = LOG(GAMMA(P) * (GAMMA(Q)/GAMMA(P+Q)) )
      RETURN
C
C P IS SMALL, BUT Q IS BIG.
C
 20   CORR = R9LGMC(Q) - R9LGMC(P+Q)
      ALBETA = ALNGAM(P) + CORR + P - P*LOG(P+Q) +
     1  (Q-0.5)*ALNREL(-P/(P+Q))
      RETURN
C
C P AND Q ARE BIG.
C
 30   CORR = R9LGMC(P) + R9LGMC(Q) - R9LGMC(P+Q)
      ALBETA = -0.5*LOG(Q) + SQ2PIL + CORR + (P-0.5)*LOG(P/(P+Q))
     1  + Q*ALNREL(-P/(P+Q))
      RETURN
C
      END

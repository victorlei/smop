*DECK R9LGMC
      FUNCTION R9LGMC (X)
C***BEGIN PROLOGUE  R9LGMC
C***SUBSIDIARY
C***PURPOSE  Compute the log Gamma correction factor so that
C            LOG(GAMMA(X)) = LOG(SQRT(2*PI)) + (X-.5)*LOG(X) - X
C            + R9LGMC(X).
C***LIBRARY   SLATEC (FNLIB)
C***CATEGORY  C7E
C***TYPE      SINGLE PRECISION (R9LGMC-S, D9LGMC-D, C9LGMC-C)
C***KEYWORDS  COMPLETE GAMMA FUNCTION, CORRECTION TERM, FNLIB,
C             LOG GAMMA, LOGARITHM, SPECIAL FUNCTIONS
C***AUTHOR  Fullerton, W., (LANL)
C***DESCRIPTION
C
C Compute the log gamma correction factor for X .GE. 10.0 so that
C  LOG (GAMMA(X)) = LOG(SQRT(2*PI)) + (X-.5)*LOG(X) - X + R9LGMC(X)
C
C Series for ALGM       on the interval  0.          to  1.00000D-02
C                                        with weighted error   3.40E-16
C                                         log weighted error  15.47
C                               significant figures required  14.39
C                                    decimal places required  15.86
C
C***REFERENCES  (NONE)
C***ROUTINES CALLED  CSEVL, INITS, R1MACH, XERMSG
C***REVISION HISTORY  (YYMMDD)
C   770801  DATE WRITTEN
C   890531  Changed all specific intrinsics to generic.  (WRB)
C   890531  REVISION DATE from Version 3.2
C   891214  Prologue converted to Version 4.0 format.  (BAB)
C   900315  CALLs to XERROR changed to CALLs to XERMSG.  (THJ)
C   900720  Routine changed from user-callable to subsidiary.  (WRB)
C***END PROLOGUE  R9LGMC
      DIMENSION ALGMCS(6)
      LOGICAL FIRST
      SAVE ALGMCS, NALGM, XBIG, XMAX, FIRST
      DATA ALGMCS( 1) /    .1666389480 45186E0 /
      DATA ALGMCS( 2) /   -.0000138494 817606E0 /
      DATA ALGMCS( 3) /    .0000000098 108256E0 /
      DATA ALGMCS( 4) /   -.0000000000 180912E0 /
      DATA ALGMCS( 5) /    .0000000000 000622E0 /
      DATA ALGMCS( 6) /   -.0000000000 000003E0 /
      DATA FIRST /.TRUE./
C***FIRST EXECUTABLE STATEMENT  R9LGMC
      IF (FIRST) THEN
         NALGM = INITS (ALGMCS, 6, R1MACH(3))
         XBIG = 1.0/SQRT(R1MACH(3))
         XMAX = EXP (MIN(LOG(R1MACH(2)/12.0), -LOG(12.0*R1MACH(1))) )
      ENDIF
      FIRST = .FALSE.
C
      IF (X .LT. 10.0) CALL XERMSG ('SLATEC', 'R9LGMC',
     +   'X MUST BE GE 10', 1, 2)
      IF (X.GE.XMAX) GO TO 20
C
      R9LGMC = 1.0/(12.0*X)
      IF (X.LT.XBIG) R9LGMC = CSEVL (2.0*(10./X)**2-1., ALGMCS, NALGM)/X
      RETURN
C
 20   R9LGMC = 0.0
      CALL XERMSG ('SLATEC', 'R9LGMC', 'X SO BIG R9LGMC UNDERFLOWS', 2,
     +   1)
      RETURN
C
      END

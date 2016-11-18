*DECK ATANH
      FUNCTION ATANH (X)
C***BEGIN PROLOGUE  ATANH
C***PURPOSE  Compute the arc hyperbolic tangent.
C***LIBRARY   SLATEC (FNLIB)
C***CATEGORY  C4C
C***TYPE      SINGLE PRECISION (ATANH-S, DATANH-D, CATANH-C)
C***KEYWORDS  ARC HYPERBOLIC TANGENT, ATANH, ELEMENTARY FUNCTIONS,
C             FNLIB, INVERSE HYPERBOLIC TANGENT
C***AUTHOR  Fullerton, W., (LANL)
C***DESCRIPTION
C
C ATANH(X) computes the arc hyperbolic tangent of X.
C
C Series for ATNH       on the interval  0.          to  2.50000D-01
C                                        with weighted error   6.70E-18
C                                         log weighted error  17.17
C                               significant figures required  16.01
C                                    decimal places required  17.76
C
C***REFERENCES  (NONE)
C***ROUTINES CALLED  CSEVL, INITS, R1MACH, XERMSG
C***REVISION HISTORY  (YYMMDD)
C   770401  DATE WRITTEN
C   890531  Changed all specific intrinsics to generic.  (WRB)
C   890531  REVISION DATE from Version 3.2
C   891214  Prologue converted to Version 4.0 format.  (BAB)
C   900315  CALLs to XERROR changed to CALLs to XERMSG.  (THJ)
C   900326  Removed duplicate information from DESCRIPTION section.
C           (WRB)
C***END PROLOGUE  ATANH
      DIMENSION ATNHCS(15)
      LOGICAL FIRST
      SAVE ATNHCS, NTERMS, DXREL, SQEPS, FIRST
      DATA ATNHCS( 1) /    .0943951023 93195492E0 /
      DATA ATNHCS( 2) /    .0491984370 55786159E0 /
      DATA ATNHCS( 3) /    .0021025935 22455432E0 /
      DATA ATNHCS( 4) /    .0001073554 44977611E0 /
      DATA ATNHCS( 5) /    .0000059782 67249293E0 /
      DATA ATNHCS( 6) /    .0000003505 06203088E0 /
      DATA ATNHCS( 7) /    .0000000212 63743437E0 /
      DATA ATNHCS( 8) /    .0000000013 21694535E0 /
      DATA ATNHCS( 9) /    .0000000000 83658755E0 /
      DATA ATNHCS(10) /    .0000000000 05370503E0 /
      DATA ATNHCS(11) /    .0000000000 00348665E0 /
      DATA ATNHCS(12) /    .0000000000 00022845E0 /
      DATA ATNHCS(13) /    .0000000000 00001508E0 /
      DATA ATNHCS(14) /    .0000000000 00000100E0 /
      DATA ATNHCS(15) /    .0000000000 00000006E0 /
      DATA FIRST /.TRUE./
C***FIRST EXECUTABLE STATEMENT  ATANH
      IF (FIRST) THEN
         NTERMS = INITS (ATNHCS, 15, 0.1*R1MACH(3))
         DXREL = SQRT (R1MACH(4))
         SQEPS = SQRT (3.0*R1MACH(3))
      ENDIF
      FIRST = .FALSE.
C
      Y = ABS(X)
      IF (Y .GE. 1.0) THEN
         IF (Y .GT. 1.0) THEN
            ATANH = (X - X) / (X - X)
         ELSE
            ATANH = X / 0.0
         ENDIF
         RETURN
      ENDIF
C
      IF (1.0-Y .LT. DXREL) CALL XERMSG ('SLATEC', 'ATANH',
     +   'ANSWER LT HALF PRECISION BECAUSE ABS(X) TOO NEAR 1', 1, 1)
C
      ATANH = X
      IF (Y.GT.SQEPS .AND. Y.LE.0.5) ATANH = X*(1.0 + CSEVL (8.*X*X-1.,
     1  ATNHCS, NTERMS))
      IF (Y.GT.0.5) ATANH = 0.5*LOG((1.0+X)/(1.0-X))
C
      RETURN
      END

*DECK ERF
      FUNCTION ERF (X)
C***BEGIN PROLOGUE  ERF
C***PURPOSE  Compute the error function.
C***LIBRARY   SLATEC (FNLIB)
C***CATEGORY  C8A, L5A1E
C***TYPE      SINGLE PRECISION (ERF-S, DERF-D)
C***KEYWORDS  ERF, ERROR FUNCTION, FNLIB, SPECIAL FUNCTIONS
C***AUTHOR  Fullerton, W., (LANL)
C***DESCRIPTION
C
C ERF(X) calculates the single precision error function for
C single precision argument X.
C
C Series for ERF        on the interval  0.          to  1.00000D+00
C                                        with weighted error   7.10E-18
C                                         log weighted error  17.15
C                               significant figures required  16.31
C                                    decimal places required  17.71
C
C***REFERENCES  (NONE)
C***ROUTINES CALLED  CSEVL, ERFC, INITS, R1MACH
C***REVISION HISTORY  (YYMMDD)
C   770401  DATE WRITTEN
C   890531  Changed all specific intrinsics to generic.  (WRB)
C   890531  REVISION DATE from Version 3.2
C   891214  Prologue converted to Version 4.0 format.  (BAB)
C   900727  Added EXTERNAL statement.  (WRB)
C   920618  Removed space from variable name.  (RWC, WRB)
C***END PROLOGUE  ERF
      DIMENSION ERFCS(13)
      LOGICAL FIRST
      EXTERNAL ERFC
      SAVE ERFCS, SQRTPI, NTERF, XBIG, SQEPS, FIRST
      DATA ERFCS( 1) /   -.0490461212 34691808E0 /
      DATA ERFCS( 2) /   -.1422612051 0371364E0 /
      DATA ERFCS( 3) /    .0100355821 87599796E0 /
      DATA ERFCS( 4) /   -.0005768764 69976748E0 /
      DATA ERFCS( 5) /    .0000274199 31252196E0 /
      DATA ERFCS( 6) /   -.0000011043 17550734E0 /
      DATA ERFCS( 7) /    .0000000384 88755420E0 /
      DATA ERFCS( 8) /   -.0000000011 80858253E0 /
      DATA ERFCS( 9) /    .0000000000 32334215E0 /
      DATA ERFCS(10) /   -.0000000000 00799101E0 /
      DATA ERFCS(11) /    .0000000000 00017990E0 /
      DATA ERFCS(12) /   -.0000000000 00000371E0 /
      DATA ERFCS(13) /    .0000000000 00000007E0 /
      DATA SQRTPI /1.772453850 9055160E0/
      DATA FIRST /.TRUE./
C***FIRST EXECUTABLE STATEMENT  ERF
      IF (FIRST) THEN
         NTERF = INITS (ERFCS, 13, 0.1*R1MACH(3))
         XBIG = SQRT(-LOG(SQRTPI*R1MACH(3)))
         SQEPS = SQRT(2.0*R1MACH(3))
      ENDIF
      FIRST = .FALSE.
C
      Y = ABS(X)
      IF (Y.GT.1.) GO TO 20
C
C ERF(X) = 1. - ERFC(X) FOR -1. .LE. X .LE. 1.
C
      IF (Y.LE.SQEPS) ERF = 2.0*X/SQRTPI
      IF (Y.GT.SQEPS) ERF = X*(1.0 + CSEVL(2.*X**2-1., ERFCS, NTERF))
      RETURN
C
C ERF(X) = 1. - ERFC(X) FOR  ABS(X) .GT. 1.
C
 20   IF (Y.LE.XBIG) ERF = SIGN (1.0-ERFC(Y), X)
      IF (Y.GT.XBIG) ERF = SIGN (1.0, X)
C
      RETURN
      END

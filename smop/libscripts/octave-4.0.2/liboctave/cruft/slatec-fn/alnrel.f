*DECK ALNREL
      FUNCTION ALNREL (X)
C***BEGIN PROLOGUE  ALNREL
C***PURPOSE  Evaluate ln(1+X) accurate in the sense of relative error.
C***LIBRARY   SLATEC (FNLIB)
C***CATEGORY  C4B
C***TYPE      SINGLE PRECISION (ALNREL-S, DLNREL-D, CLNREL-C)
C***KEYWORDS  ELEMENTARY FUNCTIONS, FNLIB, LOGARITHM
C***AUTHOR  Fullerton, W., (LANL)
C***DESCRIPTION
C
C ALNREL(X) evaluates ln(1+X) accurately in the sense of relative
C error when X is very small.  This routine must be used to
C maintain relative error accuracy whenever X is small and
C accurately known.
C
C Series for ALNR       on the interval -3.75000D-01 to  3.75000D-01
C                                        with weighted error   1.93E-17
C                                         log weighted error  16.72
C                               significant figures required  16.44
C                                    decimal places required  17.40
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
C***END PROLOGUE  ALNREL
      DIMENSION ALNRCS(23)
      LOGICAL FIRST
      SAVE ALNRCS, NLNREL, XMIN, FIRST
      DATA ALNRCS( 1) /   1.0378693562 743770E0 /
      DATA ALNRCS( 2) /   -.1336430150 4908918E0 /
      DATA ALNRCS( 3) /    .0194082491 35520563E0 /
      DATA ALNRCS( 4) /   -.0030107551 12753577E0 /
      DATA ALNRCS( 5) /    .0004869461 47971548E0 /
      DATA ALNRCS( 6) /   -.0000810548 81893175E0 /
      DATA ALNRCS( 7) /    .0000137788 47799559E0 /
      DATA ALNRCS( 8) /   -.0000023802 21089435E0 /
      DATA ALNRCS( 9) /    .0000004164 04162138E0 /
      DATA ALNRCS(10) /   -.0000000735 95828378E0 /
      DATA ALNRCS(11) /    .0000000131 17611876E0 /
      DATA ALNRCS(12) /   -.0000000023 54670931E0 /
      DATA ALNRCS(13) /    .0000000004 25227732E0 /
      DATA ALNRCS(14) /   -.0000000000 77190894E0 /
      DATA ALNRCS(15) /    .0000000000 14075746E0 /
      DATA ALNRCS(16) /   -.0000000000 02576907E0 /
      DATA ALNRCS(17) /    .0000000000 00473424E0 /
      DATA ALNRCS(18) /   -.0000000000 00087249E0 /
      DATA ALNRCS(19) /    .0000000000 00016124E0 /
      DATA ALNRCS(20) /   -.0000000000 00002987E0 /
      DATA ALNRCS(21) /    .0000000000 00000554E0 /
      DATA ALNRCS(22) /   -.0000000000 00000103E0 /
      DATA ALNRCS(23) /    .0000000000 00000019E0 /
      DATA FIRST /.TRUE./
C***FIRST EXECUTABLE STATEMENT  ALNREL
      IF (FIRST) THEN
         NLNREL = INITS (ALNRCS, 23, 0.1*R1MACH(3))
         XMIN = -1.0 + SQRT(R1MACH(4))
      ENDIF
      FIRST = .FALSE.
C
      IF (X .LE. (-1.0)) CALL XERMSG ('SLATEC', 'ALNREL', 'X IS LE -1',
     +   2, 2)
      IF (X .LT. XMIN) CALL XERMSG ('SLATEC', 'ALNREL',
     +   'ANSWER LT HALF PRECISION BECAUSE X TOO NEAR -1', 1, 1)
C
      IF (ABS(X).LE.0.375) ALNREL = X*(1. -
     1  X*CSEVL (X/.375, ALNRCS, NLNREL))
      IF (ABS(X).GT.0.375) ALNREL = LOG (1.0+X)
C
      RETURN
      END

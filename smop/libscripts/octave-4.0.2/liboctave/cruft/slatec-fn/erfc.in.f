*DECK ERFC
      FUNCTION ERFC (X)
C***BEGIN PROLOGUE  ERFC
C***PURPOSE  Compute the complementary error function.
C***LIBRARY   SLATEC (FNLIB)
C***CATEGORY  C8A, L5A1E
C***TYPE      SINGLE PRECISION (ERFC-S, DERFC-D)
C***KEYWORDS  COMPLEMENTARY ERROR FUNCTION, ERFC, FNLIB,
C             SPECIAL FUNCTIONS
C***AUTHOR  Fullerton, W., (LANL)
C***DESCRIPTION
C
C ERFC(X) calculates the single precision complementary error
C function for single precision argument X.
C
C Series for ERF        on the interval  0.          to  1.00000D+00
C                                        with weighted error   7.10E-18
C                                         log weighted error  17.15
C                               significant figures required  16.31
C                                    decimal places required  17.71
C
C Series for ERFC       on the interval  0.          to  2.50000D-01
C                                        with weighted error   4.81E-17
C                                         log weighted error  16.32
C                        approx significant figures required  15.0
C
C
C Series for ERC2       on the interval  2.50000D-01 to  1.00000D+00
C                                        with weighted error   5.22E-17
C                                         log weighted error  16.28
C                        approx significant figures required  15.0
C                                    decimal places required  16.96
C
C***REFERENCES  (NONE)
C***ROUTINES CALLED  CSEVL, INITS, R1MACH, XERMSG
C***REVISION HISTORY  (YYMMDD)
C   770701  DATE WRITTEN
C   890531  Changed all specific intrinsics to generic.  (WRB)
C   890531  REVISION DATE from Version 3.2
C   891214  Prologue converted to Version 4.0 format.  (BAB)
C   900315  CALLs to XERROR changed to CALLs to XERMSG.  (THJ)
C   920618  Removed space from variable names.  (RWC, WRB)
C***END PROLOGUE  ERFC
      DIMENSION ERFCS(13), ERFCCS(24), ERC2CS(23)
      LOGICAL FIRST
      SAVE ERFCS, ERC2CS, ERFCCS, SQRTPI, NTERF, NTERFC,
     1 NTERC2, XSML, XMAX, SQEPS, FIRST
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
      DATA ERC2CS( 1) /   -.0696013466 02309501E0 /
      DATA ERC2CS( 2) /   -.0411013393 62620893E0 /
      DATA ERC2CS( 3) /    .0039144958 66689626E0 /
      DATA ERC2CS( 4) /   -.0004906395 65054897E0 /
      DATA ERC2CS( 5) /    .0000715747 90013770E0 /
      DATA ERC2CS( 6) /   -.0000115307 16341312E0 /
      DATA ERC2CS( 7) /    .0000019946 70590201E0 /
      DATA ERC2CS( 8) /   -.0000003642 66647159E0 /
      DATA ERC2CS( 9) /    .0000000694 43726100E0 /
      DATA ERC2CS(10) /   -.0000000137 12209021E0 /
      DATA ERC2CS(11) /    .0000000027 88389661E0 /
      DATA ERC2CS(12) /   -.0000000005 81416472E0 /
      DATA ERC2CS(13) /    .0000000001 23892049E0 /
      DATA ERC2CS(14) /   -.0000000000 26906391E0 /
      DATA ERC2CS(15) /    .0000000000 05942614E0 /
      DATA ERC2CS(16) /   -.0000000000 01332386E0 /
      DATA ERC2CS(17) /    .0000000000 00302804E0 /
      DATA ERC2CS(18) /   -.0000000000 00069666E0 /
      DATA ERC2CS(19) /    .0000000000 00016208E0 /
      DATA ERC2CS(20) /   -.0000000000 00003809E0 /
      DATA ERC2CS(21) /    .0000000000 00000904E0 /
      DATA ERC2CS(22) /   -.0000000000 00000216E0 /
      DATA ERC2CS(23) /    .0000000000 00000052E0 /
      DATA ERFCCS( 1) /   0.0715179310 202925E0 /
      DATA ERFCCS( 2) /   -.0265324343 37606719E0 /
      DATA ERFCCS( 3) /    .0017111539 77920853E0 /
      DATA ERFCCS( 4) /   -.0001637516 63458512E0 /
      DATA ERFCCS( 5) /    .0000198712 93500549E0 /
      DATA ERFCCS( 6) /   -.0000028437 12412769E0 /
      DATA ERFCCS( 7) /    .0000004606 16130901E0 /
      DATA ERFCCS( 8) /   -.0000000822 77530261E0 /
      DATA ERFCCS( 9) /    .0000000159 21418724E0 /
      DATA ERFCCS(10) /   -.0000000032 95071356E0 /
      DATA ERFCCS(11) /    .0000000007 22343973E0 /
      DATA ERFCCS(12) /   -.0000000001 66485584E0 /
      DATA ERFCCS(13) /    .0000000000 40103931E0 /
      DATA ERFCCS(14) /   -.0000000000 10048164E0 /
      DATA ERFCCS(15) /    .0000000000 02608272E0 /
      DATA ERFCCS(16) /   -.0000000000 00699105E0 /
      DATA ERFCCS(17) /    .0000000000 00192946E0 /
      DATA ERFCCS(18) /   -.0000000000 00054704E0 /
      DATA ERFCCS(19) /    .0000000000 00015901E0 /
      DATA ERFCCS(20) /   -.0000000000 00004729E0 /
      DATA ERFCCS(21) /    .0000000000 00001432E0 /
      DATA ERFCCS(22) /   -.0000000000 00000439E0 /
      DATA ERFCCS(23) /    .0000000000 00000138E0 /
      DATA ERFCCS(24) /   -.0000000000 00000048E0 /
      DATA SQRTPI /1.772453850 9055160E0/
      DATA FIRST /.TRUE./
C***FIRST EXECUTABLE STATEMENT  ERFC
      IF (FIRST) THEN
         ETA = 0.1*R1MACH(3)
         NTERF = INITS (ERFCS, 13, ETA)
         NTERFC = INITS (ERFCCS, 24, ETA)
         NTERC2 = INITS (ERC2CS, 23, ETA)
C
         XSML = -SQRT (-LOG(SQRTPI*R1MACH(3)))
         TXMAX = SQRT (-LOG(SQRTPI*R1MACH(1)))
         XMAX = TXMAX - 0.5*LOG(TXMAX)/TXMAX - 0.01
         SQEPS = SQRT (2.0*R1MACH(3))
      ENDIF
      FIRST = .FALSE.
C
      IF (ISNAN(X)) THEN
         ERFC = X
         RETURN
      ENDIF
C
      IF (X.GT.XSML) GO TO 20
C
C ERFC(X) = 1.0 - ERF(X) FOR X .LT. XSML
C
      ERFC = 2.
      RETURN
C
 20   IF (X.GT.XMAX) GO TO 40
      Y = ABS(X)
      IF (Y.GT.1.0) GO TO 30
C
C ERFC(X) = 1.0 - ERF(X) FOR -1. .LE. X .LE. 1.
C
      IF (Y.LT.SQEPS) ERFC = 1.0 - 2.0*X/SQRTPI
      IF (Y.GE.SQEPS) ERFC = 1.0 -
     1  X*(1.0 + CSEVL (2.*X*X-1., ERFCS, NTERF) )
      RETURN
C
C ERFC(X) = 1.0 - ERF(X) FOR 1. .LT. ABS(X) .LE. XMAX
C
 30   Y = Y*Y
      IF (Y.LE.4.) ERFC = EXP(-Y)/ABS(X) * (0.5 + CSEVL ((8./Y-5.)/3.,
     1  ERC2CS, NTERC2) )
      IF (Y.GT.4.) ERFC = EXP(-Y)/ABS(X) * (0.5 + CSEVL (8./Y-1.,
     1  ERFCCS, NTERFC) )
      IF (X.LT.0.) ERFC = 2.0 - ERFC
      RETURN
C
 40   ERFC = 0.
      RETURN
C
      END

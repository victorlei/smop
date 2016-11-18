*DECK BETAI
      REAL FUNCTION BETAI (X, PIN, QIN)
C***BEGIN PROLOGUE  BETAI
C***PURPOSE  Calculate the incomplete Beta function.
C***LIBRARY   SLATEC (FNLIB)
C***CATEGORY  C7F
C***TYPE      SINGLE PRECISION (BETAI-S, DBETAI-D)
C***KEYWORDS  FNLIB, INCOMPLETE BETA FUNCTION, SPECIAL FUNCTIONS
C***AUTHOR  Fullerton, W., (LANL)
C***DESCRIPTION
C
C   BETAI calculates the REAL incomplete beta function.
C
C   The incomplete beta function ratio is the probability that a
C   random variable from a beta distribution having parameters PIN and
C   QIN will be less than or equal to X.
C
C     -- Input Arguments -- All arguments are REAL.
C   X      upper limit of integration.  X must be in (0,1) inclusive.
C   PIN    first beta distribution parameter.  PIN must be .GT. 0.0.
C   QIN    second beta distribution parameter.  QIN must be .GT. 0.0.
C
C***REFERENCES  Nancy E. Bosten and E. L. Battiste, Remark on Algorithm
C                 179, Communications of the ACM 17, 3 (March 1974),
C                 pp. 156.
C***ROUTINES CALLED  ALBETA, R1MACH, XERMSG
C***REVISION HISTORY  (YYMMDD)
C   770401  DATE WRITTEN
C   890531  Changed all specific intrinsics to generic.  (WRB)
C   890531  REVISION DATE from Version 3.2
C   891214  Prologue converted to Version 4.0 format.  (BAB)
C   900315  CALLs to XERROR changed to CALLs to XERMSG.  (THJ)
C   900326  Removed duplicate information from DESCRIPTION section.
C           (WRB)
C   920528  DESCRIPTION and REFERENCES sections revised.  (WRB)
C***END PROLOGUE  BETAI
      LOGICAL FIRST
      SAVE EPS, ALNEPS, SML, ALNSML, FIRST
      DATA FIRST /.TRUE./
C***FIRST EXECUTABLE STATEMENT  BETAI
      IF (FIRST) THEN
         EPS = R1MACH(3)
         ALNEPS = LOG(EPS)
         SML = R1MACH(1)
         ALNSML = LOG(SML)
      ENDIF
      FIRST = .FALSE.
C
      IF (X .LT. 0. .OR. X .GT. 1.0) CALL XERMSG ('SLATEC', 'BETAI',
     +   'X IS NOT IN THE RANGE (0,1)', 1, 2)
      IF (PIN .LE. 0. .OR. QIN .LE. 0.) CALL XERMSG ('SLATEC', 'BETAI',
     +   'P AND/OR Q IS LE ZERO', 2, 2)
C
      Y = X
      P = PIN
      Q = QIN
      IF (Q.LE.P .AND. X.LT.0.8) GO TO 20
      IF (X.LT.0.2) GO TO 20
      Y = 1.0 - Y
      P = QIN
      Q = PIN
C
 20   IF ((P+Q)*Y/(P+1.).LT.EPS) GO TO 80
C
C EVALUATE THE INFINITE SUM FIRST.
C TERM WILL EQUAL Y**P/BETA(PS,P) * (1.-PS)I * Y**I / FAC(I)
C
      PS = Q - AINT(Q)
      IF (PS.EQ.0.) PS = 1.0
      XB = P*LOG(Y) -  ALBETA(PS, P) - LOG(P)
      BETAI = 0.0
      IF (XB.LT.ALNSML) GO TO 40
C
      BETAI = EXP (XB)
      TERM = BETAI*P
      IF (PS.EQ.1.0) GO TO 40
C
      N = MAX (ALNEPS/LOG(Y), 4.0E0)
      DO 30 I=1,N
        TERM = TERM*(I-PS)*Y/I
        BETAI = BETAI + TERM/(P+I)
 30   CONTINUE
C
C NOW EVALUATE THE FINITE SUM, MAYBE.
C
 40   IF (Q.LE.1.0) GO TO 70
C
      XB = P*LOG(Y) + Q*LOG(1.0-Y) - ALBETA(P,Q) - LOG(Q)
      IB = MAX (XB/ALNSML, 0.0E0)
      TERM = EXP (XB - IB*ALNSML)
      C = 1.0/(1.0-Y)
      P1 = Q*C/(P+Q-1.)
C
      FINSUM = 0.0
      N = Q
      IF (Q.EQ.REAL(N)) N = N - 1
      DO 50 I=1,N
        IF (P1.LE.1.0 .AND. TERM/EPS.LE.FINSUM) GO TO 60
        TERM = (Q-I+1)*C*TERM/(P+Q-I)
C
        IF (TERM.GT.1.0) IB = IB - 1
        IF (TERM.GT.1.0) TERM = TERM*SML
C
        IF (IB.EQ.0) FINSUM = FINSUM + TERM
 50   CONTINUE
C
 60   BETAI = BETAI + FINSUM
 70   IF (Y.NE.X .OR. P.NE.PIN) BETAI = 1.0 - BETAI
      BETAI = MAX (MIN (BETAI, 1.0), 0.0)
      RETURN
C
 80   BETAI = 0.0
      XB = P*LOG(MAX(Y,SML)) - LOG(P) - ALBETA(P,Q)
      IF (XB.GT.ALNSML .AND. Y.NE.0.) BETAI = EXP (XB)
      IF (Y.NE.X .OR. P.NE.PIN) BETAI = 1.0 - BETAI
      RETURN
C
      END

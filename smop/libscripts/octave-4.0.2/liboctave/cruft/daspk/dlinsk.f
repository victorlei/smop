C Work performed under the auspices of the U.S. Department of Energy
C by Lawrence Livermore National Laboratory under contract number
C W-7405-Eng-48.
C
      SUBROUTINE DLINSK (NEQ, Y, T, YPRIME, SAVR, CJ, P, PNRM, WT,
     *   SQRTN, RSQRTN, LSOFF, STPTOL, IRET, RES, IRES, PSOL, WM, IWM,
     *   RHOK, FNRM, ICOPT, ID, WP, IWP, R, EPLIN, YNEW, YPNEW, PWK,
     *   ICNFLG, ICNSTR, RLX, RPAR, IPAR)
C
C***BEGIN PROLOGUE  DLINSK
C***REFER TO  DNSIK
C***DATE WRITTEN   940830   (YYMMDD)
C***REVISION DATE  951006   (Arguments SQRTN, RSQRTN added.)
C***REVISION DATE  960129   Moved line RL = ONE to top block.
C
C
C-----------------------------------------------------------------------
C***DESCRIPTION
C
C     DLINSK uses a linesearch algorithm to calculate a new (Y,YPRIME)
C     pair (YNEW,YPNEW) such that
C
C     f(YNEW,YPNEW) .le. (1 - 2*ALPHA*RL)*f(Y,YPRIME) +
C                          ALPHA*RL*RHOK*RHOK ,
C
C     where 0 < RL <= 1, and RHOK is the scaled preconditioned norm of
C     the final residual vector in the Krylov iteration.
C     Here, f(y,y') is defined as
C
C      f(y,y') = (1/2)*norm( (P-inverse)*G(t,y,y') )**2 ,
C
C     where norm() is the weighted RMS vector norm, G is the DAE
C     system residual function, and P is the preconditioner used
C     in the Krylov iteration.
C
C     In addition to the parameters defined elsewhere, we have
C
C     SAVR    -- Work array of length NEQ, containing the residual
C                vector G(t,y,y') on return.
C     P       -- Approximate Newton step used in backtracking.
C     PNRM    -- Weighted RMS norm of P.
C     LSOFF   -- Flag showing whether the linesearch algorithm is
C                to be invoked.  0 means do the linesearch,
C                1 means turn off linesearch.
C     STPTOL  -- Tolerance used in calculating the minimum lambda
C                value allowed.
C     ICNFLG  -- Integer scalar.  If nonzero, then constraint violations
C                in the proposed new approximate solution will be
C                checked for, and the maximum step length will be
C                adjusted accordingly.
C     ICNSTR  -- Integer array of length NEQ containing flags for
C                checking constraints.
C     RHOK    -- Weighted norm of preconditioned Krylov residual.
C     RLX     -- Real scalar restricting update size in DCNSTR.
C     YNEW    -- Array of length NEQ used to hold the new Y in
C                performing the linesearch.
C     YPNEW   -- Array of length NEQ used to hold the new YPRIME in
C                performing the linesearch.
C     PWK     -- Work vector of length NEQ for use in PSOL.
C     Y       -- Array of length NEQ containing the new Y (i.e.,=YNEW).
C     YPRIME  -- Array of length NEQ containing the new YPRIME
C                (i.e.,=YPNEW).
C     FNRM    -- Real scalar containing SQRT(2*f(Y,YPRIME)) for the
C                current (Y,YPRIME) on input and output.
C     R       -- Work space length NEQ for residual vector.
C     IRET    -- Return flag.
C                IRET=0 means that a satisfactory (Y,YPRIME) was found.
C                IRET=1 means that the routine failed to find a new
C                       (Y,YPRIME) that was sufficiently distinct from
C                       the current (Y,YPRIME) pair.
C                IRET=2 means a failure in RES or PSOL.
C-----------------------------------------------------------------------
C
C***ROUTINES CALLED
C   DFNRMK, DYYPNW, DCOPY
C
C***END PROLOGUE  DLINSK
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      EXTERNAL  RES, PSOL
      DIMENSION Y(*), YPRIME(*), P(*), WT(*), SAVR(*), R(*), ID(*)
      DIMENSION WM(*), IWM(*), YNEW(*), YPNEW(*), PWK(*), ICNSTR(*)
      DIMENSION WP(*), IWP(*), RPAR(*), IPAR(*)
      CHARACTER MSG*80
C
      PARAMETER (LNRE=12, LNPS=21, LKPRIN=31)
C
      SAVE ALPHA, ONE, TWO
      DATA ALPHA/1.0D-4/, ONE/1.0D0/, TWO/2.0D0/
C
      KPRIN=IWM(LKPRIN)
      F1NRM = (FNRM*FNRM)/TWO
      RATIO = ONE
C
      IF (KPRIN .GE. 2) THEN
        MSG = '------ IN ROUTINE DLINSK-- PNRM = (R1) )'
        CALL XERRWD(MSG, 40, 921, 0, 0, 0, 0, 1, PNRM, 0.0D0)
        ENDIF
      TAU = PNRM
      IVIO = 0
      RL = ONE
C-----------------------------------------------------------------------
C Check for violations of the constraints, if any are imposed.
C If any violations are found, the step vector P is rescaled, and the
C constraint check is repeated, until no violations are found.
C-----------------------------------------------------------------------
      IF (ICNFLG .NE. 0) THEN
 10      CONTINUE
         CALL DYYPNW (NEQ,Y,YPRIME,CJ,RL,P,ICOPT,ID,YNEW,YPNEW)
         CALL DCNSTR (NEQ, Y, YNEW, ICNSTR, TAU, RLX, IRET, IVAR)
         IF (IRET .EQ. 1) THEN
            IVIO = 1
            RATIO1 = TAU/PNRM
            RATIO = RATIO*RATIO1
            DO 20 I = 1,NEQ
 20           P(I) = P(I)*RATIO1
            PNRM = TAU
            IF (KPRIN .GE. 2) THEN
              MSG = '------ CONSTRAINT VIOL., PNRM = (R1), INDEX = (I1)'
              CALL XERRWD(MSG, 50, 922, 0, 1, IVAR, 0, 1, PNRM, 0.0D0)
              ENDIF
            IF (PNRM .LE. STPTOL) THEN
              IRET = 1
              RETURN
              ENDIF
            GO TO 10
            ENDIF
         ENDIF
C
      SLPI = (-TWO*F1NRM + RHOK*RHOK)*RATIO
      RLMIN = STPTOL/PNRM
      IF (LSOFF .EQ. 0 .AND. KPRIN .GE. 2) THEN
        MSG = '------ MIN. LAMBDA = (R1)'
        CALL XERRWD(MSG, 25, 923, 0, 0, 0, 0, 1, RLMIN, 0.0D0)
        ENDIF
C-----------------------------------------------------------------------
C Begin iteration to find RL value satisfying alpha-condition.
C Update YNEW and YPNEW, then compute norm of new scaled residual and
C perform alpha condition test.
C-----------------------------------------------------------------------
 100  CONTINUE
      CALL DYYPNW (NEQ,Y,YPRIME,CJ,RL,P,ICOPT,ID,YNEW,YPNEW)
      CALL DFNRMK (NEQ, YNEW, T, YPNEW, SAVR, R, CJ, WT, SQRTN, RSQRTN,
     *  RES, IRES, PSOL, 0, IER, FNRMP, EPLIN, WP, IWP, PWK, RPAR, IPAR)
      IWM(LNRE) = IWM(LNRE) + 1
      IF (IRES .GE. 0) IWM(LNPS) = IWM(LNPS) + 1
      IF (IRES .NE. 0 .OR. IER .NE. 0) THEN
        IRET = 2
        RETURN
        ENDIF
      IF (LSOFF .EQ. 1) GO TO 150
C
      F1NRMP = FNRMP*FNRMP/TWO
      IF (KPRIN .GE. 2) THEN
        MSG = '------ LAMBDA = (R1)'
        CALL XERRWD(MSG, 20, 924, 0, 0, 0, 0, 1, RL, 0.0D0)
        MSG = '------ NORM(F1) = (R1),  NORM(F1NEW) = (R2)'
        CALL XERRWD(MSG, 43, 925, 0, 0, 0, 0, 2, F1NRM, F1NRMP)
        ENDIF
      IF (F1NRMP .GT. F1NRM + ALPHA*SLPI*RL) GO TO 200
C-----------------------------------------------------------------------
C Alpha-condition is satisfied, or linesearch is turned off.
C Copy YNEW,YPNEW to Y,YPRIME and return.
C-----------------------------------------------------------------------
 150  IRET = 0
      CALL DCOPY(NEQ, YNEW, 1, Y, 1)
      CALL DCOPY(NEQ, YPNEW, 1, YPRIME, 1)
      FNRM = FNRMP
      IF (KPRIN .GE. 1) THEN
        MSG = '------ LEAVING ROUTINE DLINSK, FNRM = (R1)'
        CALL XERRWD(MSG, 42, 926, 0, 0, 0, 0, 1, FNRM, 0.0D0)
        ENDIF
      RETURN
C-----------------------------------------------------------------------
C Alpha-condition not satisfied.  Perform backtrack to compute new RL
C value.  If RL is less than RLMIN, i.e. no satisfactory YNEW,YPNEW can
C be found sufficiently distinct from Y,YPRIME, then return IRET = 1.
C-----------------------------------------------------------------------
 200  CONTINUE
      IF (RL .LT. RLMIN) THEN
        IRET = 1
        RETURN
        ENDIF
C
      RL = RL/TWO
      GO TO 100
C
C----------------------- END OF SUBROUTINE DLINSK ----------------------
      END

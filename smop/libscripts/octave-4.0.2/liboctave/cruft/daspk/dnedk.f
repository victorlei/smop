C Work performed under the auspices of the U.S. Department of Energy
C by Lawrence Livermore National Laboratory under contract number
C W-7405-Eng-48.
C
      SUBROUTINE DNEDK(X,Y,YPRIME,NEQ,RES,JACK,PSOL,
     *   H,WT,JSTART,IDID,RPAR,IPAR,PHI,GAMMA,SAVR,DELTA,E,
     *   WM,IWM,CJ,CJOLD,CJLAST,S,UROUND,EPLI,SQRTN,RSQRTN,
     *   EPCON,JCALC,JFLG,KP1,NONNEG,NTYPE,IERNLS)
C
C***BEGIN PROLOGUE  DNEDK
C***REFER TO  DDASPK
C***DATE WRITTEN   891219   (YYMMDD)
C***REVISION DATE  900926   (YYMMDD)
C***REVISION DATE  940701   (YYMMDD)
C
C
C-----------------------------------------------------------------------
C***DESCRIPTION
C
C     DNEDK solves a nonlinear system of
C     algebraic equations of the form
C     G(X,Y,YPRIME) = 0 for the unknown Y.
C
C     The method used is a matrix-free Newton scheme.
C
C     The parameters represent
C     X         -- Independent variable.
C     Y         -- Solution vector at x.
C     YPRIME    -- Derivative of solution vector
C                  after successful step.
C     NEQ       -- Number of equations to be integrated.
C     RES       -- External user-supplied subroutine
C                  to evaluate the residual.  See RES description
C                  in DDASPK prologue.
C     JACK     --  External user-supplied routine to update
C                  the preconditioner.  (This is optional).
C                  See JAC description for the case
C                  INFO(12) = 1 in the DDASPK prologue.
C     PSOL      -- External user-supplied routine to solve
C                  a linear system using preconditioning.
C                  (This is optional).  See explanation inside DDASPK.
C     H         -- Appropriate step size for this step.
C     WT        -- Vector of weights for error criterion.
C     JSTART    -- Indicates first call to this routine.
C                  If JSTART = 0, then this is the first call,
C                  otherwise it is not.
C     IDID      -- Completion flag, output by DNEDK.
C                  See IDID description in DDASPK prologue.
C     RPAR,IPAR -- Real and integer arrays used for communication
C                  between the calling program and external user
C                  routines.  They are not altered within DASPK.
C     PHI       -- Array of divided differences used by
C                  DNEDK.  The length is NEQ*(K+1), where
C                  K is the maximum order.
C     GAMMA     -- Array used to predict Y and YPRIME.  The length
C                  is K+1, where K is the maximum order.
C     SAVR      -- Work vector for DNEDK of length NEQ.
C     DELTA     -- Work vector for DNEDK of length NEQ.
C     E         -- Error accumulation vector for DNEDK of length NEQ.
C     WM,IWM    -- Real and integer arrays storing
C                  matrix information for linear system
C                  solvers, and various other information.
C     CJ        -- Parameter always proportional to 1/H.
C     CJOLD     -- Saves the value of CJ as of the last call to DITMD.
C                  Accounts for changes in CJ needed to
C                  decide whether to call DITMD.
C     CJLAST    -- Previous value of CJ.
C     S         -- A scalar determined by the approximate rate
C                  of convergence of the Newton iteration and used
C                  in the convergence test for the Newton iteration.
C
C                  If RATE is defined to be an estimate of the
C                  rate of convergence of the Newton iteration,
C                  then S = RATE/(1.D0-RATE).
C
C                  The closer RATE is to 0., the faster the Newton
C                  iteration is converging; the closer RATE is to 1.,
C                  the slower the Newton iteration is converging.
C
C                  On the first Newton iteration with an up-dated
C                  preconditioner S = 100.D0, Thus the initial
C                  RATE of convergence is approximately 1.
C
C                  S is preserved from call to call so that the rate
C                  estimate from a previous step can be applied to
C                  the current step.
C     UROUND    -- Unit roundoff.
C     EPLI      -- convergence test constant.
C                  See DDASPK prologue for more details.
C     SQRTN     -- Square root of NEQ.
C     RSQRTN    -- reciprical of square root of NEQ.
C     EPCON     -- Tolerance to test for convergence of the Newton
C                  iteration.
C     JCALC     -- Flag used to determine when to update
C                  the Jacobian matrix.  In general:
C
C                  JCALC = -1 ==> Call the DITMD routine to update
C                                 the Jacobian matrix.
C                  JCALC =  0 ==> Jacobian matrix is up-to-date.
C                  JCALC =  1 ==> Jacobian matrix is out-dated,
C                                 but DITMD will not be called unless
C                                 JCALC is set to -1.
C     JFLG      -- Flag showing whether a Jacobian routine is supplied.
C     KP1       -- The current order + 1;  updated across calls.
C     NONNEG    -- Flag to determine nonnegativity constraints.
C     NTYPE     -- Identification code for the DNEDK routine.
C                   1 ==> modified Newton; iterative linear solver.
C                   2 ==> modified Newton; user-supplied linear solver.
C     IERNLS    -- Error flag for nonlinear solver.
C                   0 ==> nonlinear solver converged.
C                   1 ==> recoverable error inside non-linear solver.
C                  -1 ==> unrecoverable error inside non-linear solver.
C
C     The following group of variables are passed as arguments to
C     the Newton iteration solver.  They are explained in greater detail
C     in DNSK:
C        TOLNEW, MULDEL, MAXIT, IERNEW
C
C     IERTYP -- Flag which tells whether this subroutine is correct.
C               0 ==> correct subroutine.
C               1 ==> incorrect subroutine.
C
C-----------------------------------------------------------------------
C***ROUTINES CALLED
C   RES, JACK, DDWNRM, DNSK
C
C***END PROLOGUE  DNEDK
C
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      DIMENSION Y(*),YPRIME(*),WT(*)
      DIMENSION PHI(NEQ,*),SAVR(*),DELTA(*),E(*)
      DIMENSION WM(*),IWM(*)
      DIMENSION GAMMA(*),RPAR(*),IPAR(*)
      EXTERNAL  RES, JACK, PSOL
C
      PARAMETER (LNRE=12, LNJE=13, LLOCWP=29, LLCIWP=30)
C
      SAVE MULDEL, MAXIT, XRATE
      DATA MULDEL/0/, MAXIT/4/, XRATE/0.25D0/
C
C     Verify that this is the correct subroutine.
C
      IERTYP = 0
      IF (NTYPE .NE. 1) THEN
         IERTYP = 1
         GO TO 380
         ENDIF
C
C     If this is the first step, perform initializations.
C
      IF (JSTART .EQ. 0) THEN
         CJOLD = CJ
         JCALC = -1
         S = 100.D0
         ENDIF
C
C     Perform all other initializations.
C
      IERNLS = 0
      LWP = IWM(LLOCWP)
      LIWP = IWM(LLCIWP)
C
C     Decide whether to update the preconditioner.
C
      IF (JFLG .NE. 0) THEN
         TEMP1 = (1.0D0 - XRATE)/(1.0D0 + XRATE)
         TEMP2 = 1.0D0/TEMP1
         IF (CJ/CJOLD .LT. TEMP1 .OR. CJ/CJOLD .GT. TEMP2) JCALC = -1
         IF (CJ .NE. CJLAST) S = 100.D0
      ELSE
         JCALC = 0
         ENDIF
C
C     Looping point for updating preconditioner with current stepsize.
C
300   CONTINUE
C
C     Initialize all error flags to zero.
C
      IERPJ = 0
      IRES = 0
      IERSL = 0
      IERNEW = 0
C
C     Predict the solution and derivative and compute the tolerance
C     for the Newton iteration.
C
      DO 310 I=1,NEQ
         Y(I)=PHI(I,1)
310      YPRIME(I)=0.0D0
      DO 330 J=2,KP1
         DO 320 I=1,NEQ
            Y(I)=Y(I)+PHI(I,J)
320         YPRIME(I)=YPRIME(I)+GAMMA(J)*PHI(I,J)
330   CONTINUE
      EPLIN = EPLI*EPCON
      TOLNEW = EPLIN
C
C     Call RES to initialize DELTA.
C
      IWM(LNRE)=IWM(LNRE)+1
      CALL RES(X,Y,YPRIME,CJ,DELTA,IRES,RPAR,IPAR)
      IF (IRES .LT. 0) GO TO 380
C
C
C     If indicated, update the preconditioner.
C     Set JCALC to 0 as an indicator that this has been done.
C
      IF(JCALC .EQ. -1)THEN
         IWM(LNJE) = IWM(LNJE) + 1
         JCALC=0
         CALL JACK (RES, IRES, NEQ, X, Y, YPRIME, WT, DELTA, E, H, CJ,
     *      WM(LWP), IWM(LIWP), IERPJ, RPAR, IPAR)
         CJOLD=CJ
         S = 100.D0
         IF (IRES .LT. 0)  GO TO 380
         IF (IERPJ .NE. 0) GO TO 380
      ENDIF
C
C     Call the nonlinear Newton solver.
C
      CALL DNSK(X,Y,YPRIME,NEQ,RES,PSOL,WT,RPAR,IPAR,SAVR,
     *   DELTA,E,WM,IWM,CJ,SQRTN,RSQRTN,EPLIN,EPCON,
     *   S,TEMP1,TOLNEW,MULDEL,MAXIT,IRES,IERSL,IERNEW)
C
      IF (IERNEW .GT. 0 .AND. JCALC .NE. 0) THEN
C
C     The Newton iteration had a recoverable failure with an old
C     preconditioner.  Retry the step with a new preconditioner.
C
         JCALC = -1
         GO TO 300
      ENDIF
C
      IF (IERNEW .NE. 0) GO TO 380
C
C     The Newton iteration has converged.  If nonnegativity of
C     solution is required, set the solution nonnegative, if the
C     perturbation to do it is small enough.  If the change is too
C     large, then consider the corrector iteration to have failed.
C
      IF(NONNEG .EQ. 0) GO TO 390
      DO 360 I = 1,NEQ
 360    DELTA(I) = MIN(Y(I),0.0D0)
      DELNRM = DDWNRM(NEQ,DELTA,WT,RPAR,IPAR)
      IF(DELNRM .GT. EPCON) GO TO 380
      DO 370 I = 1,NEQ
 370    E(I) = E(I) - DELTA(I)
      GO TO 390
C
C
C     Exits from nonlinear solver.
C     No convergence with current preconditioner.
C     Compute IERNLS and IDID accordingly.
C
380   CONTINUE
      IF (IRES .LE. -2 .OR. IERSL .LT. 0 .OR. IERTYP .NE. 0) THEN
         IERNLS = -1
         IF (IRES .LE. -2) IDID = -11
         IF (IERSL .LT. 0) IDID = -13
         IF (IERTYP .NE. 0) IDID = -15
      ELSE
         IERNLS =  1
         IF (IRES .EQ. -1) IDID = -10
         IF (IERPJ .NE. 0) IDID = -5
         IF (IERSL .GT. 0) IDID = -14
      ENDIF
C
C
390   JCALC = 1
      RETURN
C
C------END OF SUBROUTINE DNEDK------------------------------------------
      END

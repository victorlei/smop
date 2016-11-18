C Work performed under the auspices of the U.S. Department of Energy
C by Lawrence Livermore National Laboratory under contract number
C W-7405-Eng-48.
C
      SUBROUTINE DNEDD(X,Y,YPRIME,NEQ,RES,JACD,PDUM,H,WT,
     *   JSTART,IDID,RPAR,IPAR,PHI,GAMMA,DUMSVR,DELTA,E,
     *   WM,IWM,CJ,CJOLD,CJLAST,S,UROUND,DUME,DUMS,DUMR,
     *   EPCON,JCALC,JFDUM,KP1,NONNEG,NTYPE,IERNLS)
C
C***BEGIN PROLOGUE  DNEDD
C***REFER TO  DDASPK
C***DATE WRITTEN   891219   (YYMMDD)
C***REVISION DATE  900926   (YYMMDD)
C
C
C-----------------------------------------------------------------------
C***DESCRIPTION
C
C     DNEDD solves a nonlinear system of
C     algebraic equations of the form
C     G(X,Y,YPRIME) = 0 for the unknown Y.
C
C     The method used is a modified Newton scheme.
C
C     The parameters represent
C
C     X         -- Independent variable.
C     Y         -- Solution vector.
C     YPRIME    -- Derivative of solution vector.
C     NEQ       -- Number of unknowns.
C     RES       -- External user-supplied subroutine
C                  to evaluate the residual.  See RES description
C                  in DDASPK prologue.
C     JACD      -- External user-supplied routine to evaluate the
C                  Jacobian.  See JAC description for the case
C                  INFO(12) = 0 in the DDASPK prologue.
C     PDUM      -- Dummy argument.
C     H         -- Appropriate step size for next step.
C     WT        -- Vector of weights for error criterion.
C     JSTART    -- Indicates first call to this routine.
C                  If JSTART = 0, then this is the first call,
C                  otherwise it is not.
C     IDID      -- Completion flag, output by DNEDD.
C                  See IDID description in DDASPK prologue.
C     RPAR,IPAR -- Real and integer arrays used for communication
C                  between the calling program and external user
C                  routines.  They are not altered within DASPK.
C     PHI       -- Array of divided differences used by
C                  DNEDD.  The length is NEQ*(K+1),where
C                  K is the maximum order.
C     GAMMA     -- Array used to predict Y and YPRIME.  The length
C                  is MAXORD+1 where MAXORD is the maximum order.
C     DUMSVR    -- Dummy argument.
C     DELTA     -- Work vector for NLS of length NEQ.
C     E         -- Error accumulation vector for NLS of length NEQ.
C     WM,IWM    -- Real and integer arrays storing
C                  matrix information such as the matrix
C                  of partial derivatives, permutation
C                  vector, and various other information.
C     CJ        -- Parameter always proportional to 1/H.
C     CJOLD     -- Saves the value of CJ as of the last call to DMATD.
C                  Accounts for changes in CJ needed to
C                  decide whether to call DMATD.
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
C     DUME      -- Dummy argument.
C     DUMS      -- Dummy argument.
C     DUMR      -- Dummy argument.
C     EPCON     -- Tolerance to test for convergence of the Newton
C                  iteration.
C     JCALC     -- Flag used to determine when to update
C                  the Jacobian matrix.  In general:
C
C                  JCALC = -1 ==> Call the DMATD routine to update
C                                 the Jacobian matrix.
C                  JCALC =  0 ==> Jacobian matrix is up-to-date.
C                  JCALC =  1 ==> Jacobian matrix is out-dated,
C                                 but DMATD will not be called unless
C                                 JCALC is set to -1.
C     JFDUM     -- Dummy argument.
C     KP1       -- The current order(K) + 1;  updated across calls.
C     NONNEG    -- Flag to determine nonnegativity constraints.
C     NTYPE     -- Identification code for the NLS routine.
C                   0  ==> modified Newton; direct solver.
C     IERNLS    -- Error flag for nonlinear solver.
C                   0  ==> nonlinear solver converged.
C                   1  ==> recoverable error inside nonlinear solver.
C                  -1  ==> unrecoverable error inside nonlinear solver.
C
C     All variables with "DUM" in their names are dummy variables
C     which are not used in this routine.
C
C     Following is a list and description of local variables which
C     may not have an obvious usage.  They are listed in roughly the
C     order they occur in this subroutine.
C
C     The following group of variables are passed as arguments to
C     the Newton iteration solver.  They are explained in greater detail
C     in DNSD:
C        TOLNEW, MULDEL, MAXIT, IERNEW
C
C     IERTYP -- Flag which tells whether this subroutine is correct.
C               0 ==> correct subroutine.
C               1 ==> incorrect subroutine.
C
C-----------------------------------------------------------------------
C***ROUTINES CALLED
C   DDWNRM, RES, DMATD, DNSD
C
C***END PROLOGUE  DNEDD
C
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      DIMENSION Y(*),YPRIME(*),WT(*)
      DIMENSION DELTA(*),E(*)
      DIMENSION WM(*),IWM(*), RPAR(*),IPAR(*)
      DIMENSION PHI(NEQ,*),GAMMA(*)
      EXTERNAL  RES, JACD
C
      PARAMETER (LNRE=12, LNJE=13)
C
      SAVE MULDEL, MAXIT, XRATE
      DATA MULDEL/1/, MAXIT/4/, XRATE/0.25D0/
C
C     Verify that this is the correct subroutine.
C
      IERTYP = 0
      IF (NTYPE .NE. 0) THEN
         IERTYP = 1
         GO TO 380
         ENDIF
C
C     If this is the first step, perform initializations.
C
      IF (JSTART .EQ. 0) THEN
         CJOLD = CJ
         JCALC = -1
         ENDIF
C
C     Perform all other initializations.
C
      IERNLS = 0
C
C     Decide whether new Jacobian is needed.
C
      TEMP1 = (1.0D0 - XRATE)/(1.0D0 + XRATE)
      TEMP2 = 1.0D0/TEMP1
      IF (CJ/CJOLD .LT. TEMP1 .OR. CJ/CJOLD .GT. TEMP2) JCALC = -1
      IF (CJ .NE. CJLAST) S = 100.D0
C
C-----------------------------------------------------------------------
C     Entry point for updating the Jacobian with current
C     stepsize.
C-----------------------------------------------------------------------
300   CONTINUE
C
C     Initialize all error flags to zero.
C
      IERJ = 0
      IRES = 0
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
      PNORM = DDWNRM (NEQ,Y,WT,RPAR,IPAR)
      TOLNEW = 100.D0*UROUND*PNORM
C
C     Call RES to initialize DELTA.
C
      IWM(LNRE)=IWM(LNRE)+1
      CALL RES(X,Y,YPRIME,CJ,DELTA,IRES,RPAR,IPAR)
      IF (IRES .LT. 0) GO TO 380
C
C     If indicated, reevaluate the iteration matrix
C     J = dG/dY + CJ*dG/dYPRIME (where G(X,Y,YPRIME)=0).
C     Set JCALC to 0 as an indicator that this has been done.
C
      IF(JCALC .EQ. -1) THEN
         IWM(LNJE)=IWM(LNJE)+1
         JCALC=0
         CALL DMATD(NEQ,X,Y,YPRIME,DELTA,CJ,H,IERJ,WT,E,WM,IWM,
     *              RES,IRES,UROUND,JACD,RPAR,IPAR)
         CJOLD=CJ
         S = 100.D0
         IF (IRES .LT. 0) GO TO 380
         IF(IERJ .NE. 0)GO TO 380
      ENDIF
C
C     Call the nonlinear Newton solver.
C
      TEMP1 = 2.0D0/(1.0D0 + CJ/CJOLD)
      CALL DNSD(X,Y,YPRIME,NEQ,RES,PDUM,WT,RPAR,IPAR,DUMSVR,
     *          DELTA,E,WM,IWM,CJ,DUMS,DUMR,DUME,EPCON,S,TEMP1,
     *          TOLNEW,MULDEL,MAXIT,IRES,IDUM,IERNEW)
C
      IF (IERNEW .GT. 0 .AND. JCALC .NE. 0) THEN
C
C        The Newton iteration had a recoverable failure with an old
C        iteration matrix.  Retry the step with a new iteration matrix.
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
375   IF(NONNEG .EQ. 0) GO TO 390
      DO 377 I = 1,NEQ
377      DELTA(I) = MIN(Y(I),0.0D0)
      DELNRM = DDWNRM(NEQ,DELTA,WT,RPAR,IPAR)
      IF(DELNRM .GT. EPCON) GO TO 380
      DO 378 I = 1,NEQ
378      E(I) = E(I) - DELTA(I)
      GO TO 390
C
C
C     Exits from nonlinear solver.
C     No convergence with current iteration
C     matrix, or singular iteration matrix.
C     Compute IERNLS and IDID accordingly.
C
380   CONTINUE
      IF (IRES .LE. -2 .OR. IERTYP .NE. 0) THEN
         IERNLS = -1
         IF (IRES .LE. -2) IDID = -11
         IF (IERTYP .NE. 0) IDID = -15
      ELSE
         IERNLS = 1
         IF (IRES .LT. 0) IDID = -10
         IF (IERJ .NE. 0) IDID = -8
      ENDIF
C
390   JCALC = 1
      RETURN
C
C------END OF SUBROUTINE DNEDD------------------------------------------
      END

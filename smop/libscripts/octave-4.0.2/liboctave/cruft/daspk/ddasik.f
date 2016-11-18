C Work perfored under the auspices of the U.S. Department of Energy
C by Lawrence Livermore National Laboratory under contract number
C W-7405-Eng-48.
C
      SUBROUTINE DDASIK(X,Y,YPRIME,NEQ,ICOPT,ID,RES,JACK,PSOL,H,WT,
     *   JSKIP,RPAR,IPAR,SAVR,DELTA,R,YIC,YPIC,PWK,WM,IWM,CJ,UROUND,
     *   EPLI,SQRTN,RSQRTN,EPCON,RATEMX,STPTOL,JFLG,
     *   ICNFLG,ICNSTR,IERNLS)
C
C***BEGIN PROLOGUE  DDASIK
C***REFER TO  DDASPK
C***DATE WRITTEN   941026   (YYMMDD)
C***REVISION DATE  950808   (YYMMDD)
C***REVISION DATE  951110   Removed unreachable block 390.
C
C
C-----------------------------------------------------------------------
C***DESCRIPTION
C
C
C     DDASIK solves a nonlinear system of algebraic equations of the
C     form G(X,Y,YPRIME) = 0 for the unknown parts of Y and YPRIME in
C     the initial conditions.
C
C     An initial value for Y and initial guess for YPRIME are input.
C
C     The method used is a Newton scheme with Krylov iteration and a
C     linesearch algorithm.
C
C     The parameters represent
C
C     X         -- Independent variable.
C     Y         -- Solution vector at x.
C     YPRIME    -- Derivative of solution vector.
C     NEQ       -- Number of equations to be integrated.
C     ICOPT     -- Initial condition option chosen (1 or 2).
C     ID        -- Array of dimension NEQ, which must be initialized
C                  if ICOPT = 1.  See DDASIC.
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
C     H         -- Scaling factor for this initial condition calc.
C     WT        -- Vector of weights for error criterion.
C     JSKIP     -- input flag to signal if initial JAC call is to be
C                  skipped.  1 => skip the call, 0 => do not skip call.
C     RPAR,IPAR -- Real and integer arrays used for communication
C                  between the calling program and external user
C                  routines.  They are not altered within DASPK.
C     SAVR      -- Work vector for DDASIK of length NEQ.
C     DELTA     -- Work vector for DDASIK of length NEQ.
C     R         -- Work vector for DDASIK of length NEQ.
C     YIC,YPIC  -- Work vectors for DDASIK, each of length NEQ.
C     PWK       -- Work vector for DDASIK of length NEQ.
C     WM,IWM    -- Real and integer arrays storing
C                  matrix information for linear system
C                  solvers, and various other information.
C     CJ        -- Matrix parameter = 1/H (ICOPT = 1) or 0 (ICOPT = 2).
C     UROUND    -- Unit roundoff.
C     EPLI      -- convergence test constant.
C                  See DDASPK prologue for more details.
C     SQRTN     -- Square root of NEQ.
C     RSQRTN    -- reciprical of square root of NEQ.
C     EPCON     -- Tolerance to test for convergence of the Newton
C                  iteration.
C     RATEMX    -- Maximum convergence rate for which Newton iteration
C                  is considered converging.
C     JFLG      -- Flag showing whether a Jacobian routine is supplied.
C     ICNFLG    -- Integer scalar.  If nonzero, then constraint
C                  violations in the proposed new approximate solution
C                  will be checked for, and the maximum step length
C                  will be adjusted accordingly.
C     ICNSTR    -- Integer array of length NEQ containing flags for
C                  checking constraints.
C     IERNLS    -- Error flag for nonlinear solver.
C                   0   ==> nonlinear solver converged.
C                   1,2 ==> recoverable error inside nonlinear solver.
C                           1 => retry with current Y, YPRIME
C                           2 => retry with original Y, YPRIME
C                  -1   ==> unrecoverable error in nonlinear solver.
C
C-----------------------------------------------------------------------
C
C***ROUTINES CALLED
C   RES, JACK, DNSIK, DCOPY
C
C***END PROLOGUE  DDASIK
C
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      DIMENSION Y(*),YPRIME(*),ID(*),WT(*),ICNSTR(*)
      DIMENSION SAVR(*),DELTA(*),R(*),YIC(*),YPIC(*),PWK(*)
      DIMENSION WM(*),IWM(*), RPAR(*),IPAR(*)
      EXTERNAL RES, JACK, PSOL
C
      PARAMETER (LNRE=12, LNJE=13, LLOCWP=29, LLCIWP=30)
      PARAMETER (LMXNIT=32, LMXNJ=33)
C
C
C     Perform initializations.
C
      LWP = IWM(LLOCWP)
      LIWP = IWM(LLCIWP)
      MXNIT = IWM(LMXNIT)
      MXNJ = IWM(LMXNJ)
      IERNLS = 0
      NJ = 0
      EPLIN = EPLI*EPCON
C
C     Call RES to initialize DELTA.
C
      IRES = 0
      IWM(LNRE) = IWM(LNRE) + 1
      CALL RES(X,Y,YPRIME,CJ,DELTA,IRES,RPAR,IPAR)
      IF (IRES .LT. 0) GO TO 370
C
C     Looping point for updating the preconditioner.
C
 300  CONTINUE
C
C     Initialize all error flags to zero.
C
      IERPJ = 0
      IRES = 0
      IERNEW = 0
C
C     If a Jacobian routine was supplied, call it.
C
      IF (JFLG .EQ. 1 .AND. JSKIP .EQ. 0) THEN
        NJ = NJ + 1
        IWM(LNJE)=IWM(LNJE)+1
        CALL JACK (RES, IRES, NEQ, X, Y, YPRIME, WT, DELTA, R, H, CJ,
     *     WM(LWP), IWM(LIWP), IERPJ, RPAR, IPAR)
        IF (IRES .LT. 0 .OR. IERPJ .NE. 0) GO TO 370
        ENDIF
      JSKIP = 0
C
C     Call the nonlinear Newton solver for up to MXNIT iterations.
C
      CALL DNSIK(X,Y,YPRIME,NEQ,ICOPT,ID,RES,PSOL,WT,RPAR,IPAR,
     *   SAVR,DELTA,R,YIC,YPIC,PWK,WM,IWM,CJ,SQRTN,RSQRTN,
     *   EPLIN,EPCON,RATEMX,MXNIT,STPTOL,ICNFLG,ICNSTR,IERNEW)
C
      IF (IERNEW .EQ. 1 .AND. NJ .LT. MXNJ .AND. JFLG .EQ. 1) THEN
C
C       Up to MXNIT iterations were done, the convergence rate is < 1,
C       a Jacobian routine is supplied, and the number of JACK calls
C       is less than MXNJ.
C       Copy the residual SAVR to DELTA, call JACK, and try again.
C
        CALL DCOPY (NEQ,  SAVR, 1, DELTA, 1)
        GO TO 300
        ENDIF
C
      IF (IERNEW .NE. 0) GO TO 380
      RETURN
C
C
C     Unsuccessful exits from nonlinear solver.
C     Set IERNLS accordingly.
C
 370  IERNLS = 2
      IF (IRES .LE. -2) IERNLS = -1
      RETURN
C
 380  IERNLS = MIN(IERNEW,2)
      RETURN
C
C----------------------- END OF SUBROUTINE DDASIK-----------------------
      END

C Work performed under the auspices of the U.S. Department of Energy
C by Lawrence Livermore National Laboratory under contract number
C W-7405-Eng-48.
C
      SUBROUTINE DNSIK(X,Y,YPRIME,NEQ,ICOPT,ID,RES,PSOL,WT,RPAR,IPAR,
     *   SAVR,DELTA,R,YIC,YPIC,PWK,WM,IWM,CJ,SQRTN,RSQRTN,EPLIN,EPCON,
     *   RATEMX,MAXIT,STPTOL,ICNFLG,ICNSTR,IERNEW)
C
C***BEGIN PROLOGUE  DNSIK
C***REFER TO  DDASPK
C***DATE WRITTEN   940701   (YYMMDD)
C***REVISION DATE  950714   (YYMMDD)
C
C
C-----------------------------------------------------------------------
C***DESCRIPTION
C
C     DNSIK solves a nonlinear system of algebraic equations of the
C     form G(X,Y,YPRIME) = 0 for the unknown parts of Y and YPRIME in
C     the initial conditions.
C
C     The method used is a Newton scheme combined with a linesearch
C     algorithm, using Krylov iterative linear system methods.
C
C     The parameters represent
C
C     X         -- Independent variable.
C     Y         -- Solution vector.
C     YPRIME    -- Derivative of solution vector.
C     NEQ       -- Number of unknowns.
C     ICOPT     -- Initial condition option chosen (1 or 2).
C     ID        -- Array of dimension NEQ, which must be initialized
C                  if ICOPT = 1.  See DDASIC.
C     RES       -- External user-supplied subroutine
C                  to evaluate the residual.  See RES description
C                  in DDASPK prologue.
C     PSOL      -- External user-supplied routine to solve
C                  a linear system using preconditioning.
C                  See explanation inside DDASPK.
C     WT        -- Vector of weights for error criterion.
C     RPAR,IPAR -- Real and integer arrays used for communication
C                  between the calling program and external user
C                  routines.  They are not altered within DASPK.
C     SAVR      -- Work vector for DNSIK of length NEQ.
C     DELTA     -- Residual vector on entry, and work vector of
C                  length NEQ for DNSIK.
C     R         -- Work vector for DNSIK of length NEQ.
C     YIC,YPIC  -- Work vectors for DNSIK, each of length NEQ.
C     PWK       -- Work vector for DNSIK of length NEQ.
C     WM,IWM    -- Real and integer arrays storing
C                  matrix information such as the matrix
C                  of partial derivatives, permutation
C                  vector, and various other information.
C     CJ        -- Matrix parameter = 1/H (ICOPT = 1) or 0 (ICOPT = 2).
C     SQRTN     -- Square root of NEQ.
C     RSQRTN    -- reciprical of square root of NEQ.
C     EPLIN     -- Tolerance for linear system solver.
C     EPCON     -- Tolerance to test for convergence of the Newton
C                  iteration.
C     RATEMX    -- Maximum convergence rate for which Newton iteration
C                  is considered converging.
C     MAXIT     -- Maximum allowed number of Newton iterations.
C     STPTOL    -- Tolerance used in calculating the minimum lambda
C                  value allowed.
C     ICNFLG    -- Integer scalar.  If nonzero, then constraint
C                  violations in the proposed new approximate solution
C                  will be checked for, and the maximum step length
C                  will be adjusted accordingly.
C     ICNSTR    -- Integer array of length NEQ containing flags for
C                  checking constraints.
C     IERNEW    -- Error flag for Newton iteration.
C                   0  ==> Newton iteration converged.
C                   1  ==> failed to converge, but RATE .lt. 1.
C                   2  ==> failed to converge, RATE .gt. RATEMX.
C                   3  ==> other recoverable error.
C                  -1  ==> unrecoverable error inside Newton iteration.
C-----------------------------------------------------------------------
C
C***ROUTINES CALLED
C   DFNRMK, DSLVK, DDWNRM, DLINSK, DCOPY
C
C***END PROLOGUE  DNSIK
C
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      DIMENSION Y(*),YPRIME(*),WT(*),ID(*),DELTA(*),R(*),SAVR(*)
      DIMENSION YIC(*),YPIC(*),PWK(*),WM(*),IWM(*), RPAR(*),IPAR(*)
      DIMENSION ICNSTR(*)
      EXTERNAL RES, PSOL
C
      PARAMETER (LNNI=19, LNPS=21, LLOCWP=29, LLCIWP=30)
      PARAMETER (LLSOFF=35, LSTOL=14)
C
C
C     Initializations.  M is the Newton iteration counter.
C
      LSOFF = IWM(LLSOFF)
      M = 0
      RATE = 1.0D0
      LWP = IWM(LLOCWP)
      LIWP = IWM(LLCIWP)
      RLX = 0.4D0
C
C     Save residual in SAVR.
C
      CALL DCOPY (NEQ, DELTA, 1, SAVR, 1)
C
C     Compute norm of (P-inverse)*(residual).
C
      CALL DFNRMK (NEQ, Y, X, YPRIME, SAVR, R, CJ, WT, SQRTN, RSQRTN,
     *   RES, IRES, PSOL, 1, IER, FNRM, EPLIN, WM(LWP), IWM(LIWP),
     *   PWK, RPAR, IPAR)
      IWM(LNPS) = IWM(LNPS) + 1
      IF (IER .NE. 0) THEN
        IERNEW = 3
        RETURN
      ENDIF
C
C     Return now if residual norm is .le. EPCON.
C
      IF (FNRM .LE. EPCON) RETURN
C
C     Newton iteration loop.
C
300   CONTINUE
      IWM(LNNI) = IWM(LNNI) + 1
C
C     Compute a new step vector DELTA.
C
      CALL DSLVK (NEQ, Y, X, YPRIME, SAVR, DELTA, WT, WM, IWM,
     *   RES, IRES, PSOL, IERSL, CJ, EPLIN, SQRTN, RSQRTN, RHOK,
     *   RPAR, IPAR)
      IF (IRES .NE. 0 .OR. IERSL .NE. 0) GO TO 390
C
C     Get norm of DELTA.  Return now if DELTA is zero.
C
      DELNRM = DDWNRM(NEQ,DELTA,WT,RPAR,IPAR)
      IF (DELNRM .EQ. 0.0D0) RETURN
C
C     Call linesearch routine for global strategy and set RATE.
C
      OLDFNM = FNRM
C
      CALL DLINSK (NEQ, Y, X, YPRIME, SAVR, CJ, DELTA, DELNRM, WT,
     *   SQRTN, RSQRTN, LSOFF, STPTOL, IRET, RES, IRES, PSOL, WM, IWM,
     *   RHOK, FNRM, ICOPT, ID, WM(LWP), IWM(LIWP), R, EPLIN, YIC, YPIC,
     *   PWK, ICNFLG, ICNSTR, RLX, RPAR, IPAR)
C
      RATE = FNRM/OLDFNM
C
C     Check for error condition from linesearch.
      IF (IRET .NE. 0) GO TO 390
C
C     Test for convergence of the iteration, and return or loop.
C
      IF (FNRM .LE. EPCON) RETURN
C
C     The iteration has not yet converged.  Update M.
C     Test whether the maximum number of iterations have been tried.
C
      M=M+1
      IF(M .GE. MAXIT) GO TO 380
C
C     Copy the residual SAVR to DELTA and loop for another iteration.
C
      CALL DCOPY (NEQ,  SAVR, 1, DELTA, 1)
      GO TO 300
C
C     The maximum number of iterations was done.  Set IERNEW and return.
C
380   IF (RATE .LE. RATEMX) THEN
         IERNEW = 1
      ELSE
         IERNEW = 2
      ENDIF
      RETURN
C
390   IF (IRES .LE. -2 .OR. IERSL .LT. 0) THEN
         IERNEW = -1
      ELSE
         IERNEW = 3
         IF (IRES .EQ. 0 .AND. IERSL .EQ. 1 .AND. M .GE. 2
     1       .AND. RATE .LT. 1.0D0) IERNEW = 1
      ENDIF
      RETURN
C
C
C----------------------- END OF SUBROUTINE DNSIK------------------------
      END

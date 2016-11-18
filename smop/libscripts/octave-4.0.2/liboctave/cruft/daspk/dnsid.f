C Work performed under the auspices of the U.S. Department of Energy
C by Lawrence Livermore National Laboratory under contract number
C W-7405-Eng-48.
C
      SUBROUTINE DNSID(X,Y,YPRIME,NEQ,ICOPT,ID,RES,WT,RPAR,IPAR,
     *   DELTA,R,YIC,YPIC,WM,IWM,CJ,EPCON,RATEMX,MAXIT,STPTOL,
     *   ICNFLG,ICNSTR,IERNEW)
C
C***BEGIN PROLOGUE  DNSID
C***REFER TO  DDASPK
C***DATE WRITTEN   940701   (YYMMDD)
C***REVISION DATE  950713   (YYMMDD)
C
C
C-----------------------------------------------------------------------
C***DESCRIPTION
C
C     DNSID solves a nonlinear system of algebraic equations of the
C     form G(X,Y,YPRIME) = 0 for the unknown parts of Y and YPRIME
C     in the initial conditions.
C
C     The method used is a modified Newton scheme.
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
C     RES       -- External user-supplied subroutine to evaluate the
C                  residual.  See RES description in DDASPK prologue.
C     WT        -- Vector of weights for error criterion.
C     RPAR,IPAR -- Real and integer arrays used for communication
C                  between the calling program and external user
C                  routines.  They are not altered within DASPK.
C     DELTA     -- Residual vector on entry, and work vector of
C                  length NEQ for DNSID.
C     WM,IWM    -- Real and integer arrays storing matrix information
C                  such as the matrix of partial derivatives,
C                  permutation vector, and various other information.
C     CJ        -- Matrix parameter = 1/H (ICOPT = 1) or 0 (ICOPT = 2).
C     R         -- Array of length NEQ used as workspace by the
C                  linesearch routine DLINSD.
C     YIC,YPIC  -- Work vectors for DLINSD, each of length NEQ.
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
C                   1  ==> failed to converge, but RATE .le. RATEMX.
C                   2  ==> failed to converge, RATE .gt. RATEMX.
C                   3  ==> other recoverable error (IRES = -1, or
C                          linesearch failed).
C                  -1  ==> unrecoverable error (IRES = -2).
C
C-----------------------------------------------------------------------
C
C***ROUTINES CALLED
C   DSLVD, DDWNRM, DLINSD, DCOPY
C
C***END PROLOGUE  DNSID
C
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      DIMENSION Y(*),YPRIME(*),WT(*),R(*)
      DIMENSION ID(*),DELTA(*), YIC(*), YPIC(*)
      DIMENSION WM(*),IWM(*), RPAR(*),IPAR(*)
      DIMENSION ICNSTR(*)
      EXTERNAL  RES
C
      PARAMETER (LNNI=19, LLSOFF=35)
C
C
C     Initializations.  M is the Newton iteration counter.
C
      LSOFF = IWM(LLSOFF)
      M = 0
      RATE = 1.0D0
      RLX = 0.4D0
C
C     Compute a new step vector DELTA by back-substitution.
C
      CALL DSLVD (NEQ, DELTA, WM, IWM)
C
C     Get norm of DELTA.  Return now if norm(DELTA) .le. EPCON.
C
      DELNRM = DDWNRM(NEQ,DELTA,WT,RPAR,IPAR)
      FNRM = DELNRM
      IF (FNRM .LE. EPCON) RETURN
C
C     Newton iteration loop.
C
 300  CONTINUE
      IWM(LNNI) = IWM(LNNI) + 1
C
C     Call linesearch routine for global strategy and set RATE
C
      OLDFNM = FNRM
C
      CALL DLINSD (NEQ, Y, X, YPRIME, CJ, DELTA, DELNRM, WT, LSOFF,
     *             STPTOL, IRET, RES, IRES, WM, IWM, FNRM, ICOPT, ID,
     *             R, YIC, YPIC, ICNFLG, ICNSTR, RLX, RPAR, IPAR)
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
      M = M + 1
      IF (M .GE. MAXIT) GO TO 380
C
C     Copy the residual to DELTA and its norm to DELNRM, and loop for
C     another iteration.
C
      CALL DCOPY (NEQ, R, 1, DELTA, 1)
      DELNRM = FNRM
      GO TO 300
C
C     The maximum number of iterations was done.  Set IERNEW and return.
C
 380  IF (RATE .LE. RATEMX) THEN
         IERNEW = 1
      ELSE
         IERNEW = 2
      ENDIF
      RETURN
C
 390  IF (IRES .LE. -2) THEN
         IERNEW = -1
      ELSE
         IERNEW = 3
      ENDIF
      RETURN
C
C
C------END OF SUBROUTINE DNSID------------------------------------------
      END

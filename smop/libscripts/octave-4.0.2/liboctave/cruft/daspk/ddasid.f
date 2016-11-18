C Work performed under the auspices of the U.S. Department of Energy
C by Lawrence Livermore National Laboratory under contract number
C W-7405-Eng-48.
C
      SUBROUTINE DDASID(X,Y,YPRIME,NEQ,ICOPT,ID,RES,JACD,PDUM,H,WT,
     *  JSDUM,RPAR,IPAR,DUMSVR,DELTA,R,YIC,YPIC,DUMPWK,WM,IWM,CJ,UROUND,
     *  DUME,DUMS,DUMR,EPCON,RATEMX,STPTOL,JFDUM,
     *  ICNFLG,ICNSTR,IERNLS)
C
C***BEGIN PROLOGUE  DDASID
C***REFER TO  DDASPK
C***DATE WRITTEN   940701   (YYMMDD)
C***REVISION DATE  950808   (YYMMDD)
C***REVISION DATE  951110   Removed unreachable block 390.
C
C
C-----------------------------------------------------------------------
C***DESCRIPTION
C
C
C     DDASID solves a nonlinear system of algebraic equations of the
C     form G(X,Y,YPRIME) = 0 for the unknown parts of Y and YPRIME in
C     the initial conditions.
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
C     JACD      -- External user-supplied routine to evaluate the
C                  Jacobian.  See JAC description for the case
C                  INFO(12) = 0 in the DDASPK prologue.
C     PDUM      -- Dummy argument.
C     H         -- Scaling factor for this initial condition calc.
C     WT        -- Vector of weights for error criterion.
C     JSDUM     -- Dummy argument.
C     RPAR,IPAR -- Real and integer arrays used for communication
C                  between the calling program and external user
C                  routines.  They are not altered within DASPK.
C     DUMSVR    -- Dummy argument.
C     DELTA     -- Work vector for NLS of length NEQ.
C     R         -- Work vector for NLS of length NEQ.
C     YIC,YPIC  -- Work vectors for NLS, each of length NEQ.
C     DUMPWK    -- Dummy argument.
C     WM,IWM    -- Real and integer arrays storing matrix information
C                  such as the matrix of partial derivatives,
C                  permutation vector, and various other information.
C     CJ        -- Matrix parameter = 1/H (ICOPT = 1) or 0 (ICOPT = 2).
C     UROUND    -- Unit roundoff.
C     DUME      -- Dummy argument.
C     DUMS      -- Dummy argument.
C     DUMR      -- Dummy argument.
C     EPCON     -- Tolerance to test for convergence of the Newton
C                  iteration.
C     RATEMX    -- Maximum convergence rate for which Newton iteration
C                  is considered converging.
C     JFDUM     -- Dummy argument.
C     STPTOL    -- Tolerance used in calculating the minimum lambda
C                  value allowed.
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
C     All variables with "DUM" in their names are dummy variables
C     which are not used in this routine.
C
C-----------------------------------------------------------------------
C
C***ROUTINES CALLED
C   RES, DMATD, DNSID
C
C***END PROLOGUE  DDASID
C
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      DIMENSION Y(*),YPRIME(*),ID(*),WT(*),ICNSTR(*)
      DIMENSION DELTA(*),R(*),YIC(*),YPIC(*)
      DIMENSION WM(*),IWM(*), RPAR(*),IPAR(*)
      EXTERNAL  RES, JACD
C
      PARAMETER (LNRE=12, LNJE=13, LMXNIT=32, LMXNJ=33)
C
C
C     Perform initializations.
C
      MXNIT = IWM(LMXNIT)
      MXNJ = IWM(LMXNJ)
      IERNLS = 0
      NJ = 0
C
C     Call RES to initialize DELTA.
C
      IRES = 0
      IWM(LNRE) = IWM(LNRE) + 1
      CALL RES(X,Y,YPRIME,CJ,DELTA,IRES,RPAR,IPAR)
      IF (IRES .LT. 0) GO TO 370
C
C     Looping point for updating the Jacobian.
C
300   CONTINUE
C
C     Initialize all error flags to zero.
C
      IERJ = 0
      IRES = 0
      IERNEW = 0
C
C     Reevaluate the iteration matrix, J = dG/dY + CJ*dG/dYPRIME,
C     where G(X,Y,YPRIME) = 0.
C
      NJ = NJ + 1
      IWM(LNJE)=IWM(LNJE)+1
      CALL DMATD(NEQ,X,Y,YPRIME,DELTA,CJ,H,IERJ,WT,R,
     *              WM,IWM,RES,IRES,UROUND,JACD,RPAR,IPAR)
      IF (IRES .LT. 0 .OR. IERJ .NE. 0) GO TO 370
C
C     Call the nonlinear Newton solver for up to MXNIT iterations.
C
      CALL DNSID(X,Y,YPRIME,NEQ,ICOPT,ID,RES,WT,RPAR,IPAR,DELTA,R,
     *     YIC,YPIC,WM,IWM,CJ,EPCON,RATEMX,MXNIT,STPTOL,
     *     ICNFLG,ICNSTR,IERNEW)
C
      IF (IERNEW .EQ. 1 .AND. NJ .LT. MXNJ) THEN
C
C        MXNIT iterations were done, the convergence rate is < 1,
C        and the number of Jacobian evaluations is less than MXNJ.
C        Call RES, reevaluate the Jacobian, and try again.
C
         IWM(LNRE)=IWM(LNRE)+1
         CALL RES(X,Y,YPRIME,CJ,DELTA,IRES,RPAR,IPAR)
         IF (IRES .LT. 0) GO TO 370
         GO TO 300
         ENDIF
C
      IF (IERNEW .NE. 0) GO TO 380

      RETURN
C
C
C     Unsuccessful exits from nonlinear solver.
C     Compute IERNLS accordingly.
C
370   IERNLS = 2
      IF (IRES .LE. -2) IERNLS = -1
      RETURN
C
380   IERNLS = MIN(IERNEW,2)
      RETURN
C
C------END OF SUBROUTINE DDASID-----------------------------------------
      END

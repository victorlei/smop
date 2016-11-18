C Work performed under the auspices of the U.S. Department of Energy
C by Lawrence Livermore National Laboratory under contract number
C W-7405-Eng-48.
C
      SUBROUTINE DDASIC (X, Y, YPRIME, NEQ, ICOPT, ID, RES, JAC, PSOL,
     *   H, WT, NIC, IDID, RPAR, IPAR, PHI, SAVR, DELTA, E, YIC, YPIC,
     *   PWK, WM, IWM, HMIN, UROUND, EPLI, SQRTN, RSQRTN, EPCONI,
     *   STPTOL, JFLG, ICNFLG, ICNSTR, NLSIC)
C
C***BEGIN PROLOGUE  DDASIC
C***REFER TO  DDASPK
C***DATE WRITTEN   940628   (YYMMDD)
C***REVISION DATE  941206   (YYMMDD)
C***REVISION DATE  950714   (YYMMDD)
C
C-----------------------------------------------------------------------
C***DESCRIPTION
C
C     DDASIC is a driver routine to compute consistent initial values
C     for Y and YPRIME.  There are two different options:
C     Denoting the differential variables in Y by Y_d, and
C     the algebraic variables by Y_a, the problem solved is either:
C     1.  Given Y_d, calculate Y_a and Y_d', or
C     2.  Given Y', calculate Y.
C     In either case, initial values for the given components
C     are input, and initial guesses for the unknown components
C     must also be provided as input.
C
C     The external routine NLSIC solves the resulting nonlinear system.
C
C     The parameters represent
C
C     X  --        Independent variable.
C     Y  --        Solution vector at X.
C     YPRIME --    Derivative of solution vector.
C     NEQ --       Number of equations to be integrated.
C     ICOPT     -- Flag indicating initial condition option chosen.
C                    ICOPT = 1 for option 1 above.
C                    ICOPT = 2 for option 2.
C     ID        -- Array of dimension NEQ, which must be initialized
C                  if option 1 is chosen.
C                    ID(i) = +1 if Y_i is a differential variable,
C                    ID(i) = -1 if Y_i is an algebraic variable.
C     RES --       External user-supplied subroutine to evaluate the
C                  residual.  See RES description in DDASPK prologue.
C     JAC --       External user-supplied routine to update Jacobian
C                  or preconditioner information in the nonlinear solver
C                  (optional).  See JAC description in DDASPK prologue.
C     PSOL --      External user-supplied routine to solve
C                  a linear system using preconditioning.
C                  See PSOL in DDASPK prologue.
C     H --         Scaling factor in iteration matrix.  DDASIC may
C                  reduce H to achieve convergence.
C     WT --        Vector of weights for error criterion.
C     NIC --       Input number of initial condition calculation call
C                  (= 1 or 2).
C     IDID --      Completion code.  See IDID in DDASPK prologue.
C     RPAR,IPAR -- Real and integer parameter arrays that
C                  are used for communication between the
C                  calling program and external user routines.
C                  They are not altered by DNSK
C     PHI --       Work space for DDASIC of length at least 2*NEQ.
C     SAVR --      Work vector for DDASIC of length NEQ.
C     DELTA --     Work vector for DDASIC of length NEQ.
C     E --         Work vector for DDASIC of length NEQ.
C     YIC,YPIC --  Work vectors for DDASIC, each of length NEQ.
C     PWK --       Work vector for DDASIC of length NEQ.
C     WM,IWM --    Real and integer arrays storing
C                  information required by the linear solver.
C     EPCONI --    Test constant for Newton iteration convergence.
C     ICNFLG --    Flag showing whether constraints on Y are to apply.
C     ICNSTR --    Integer array of length NEQ with constraint types.
C
C     The other parameters are for use internally by DDASIC.
C
C-----------------------------------------------------------------------
C***ROUTINES CALLED
C   DCOPY, NLSIC
C
C***END PROLOGUE  DDASIC
C
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      DIMENSION Y(*),YPRIME(*),ID(*),WT(*),PHI(NEQ,*)
      DIMENSION SAVR(*),DELTA(*),E(*),YIC(*),YPIC(*),PWK(*)
      DIMENSION WM(*),IWM(*), RPAR(*),IPAR(*), ICNSTR(*)
      EXTERNAL RES, JAC, PSOL, NLSIC
C
      PARAMETER (LCFN=15)
      PARAMETER (LMXNH=34)
C
C The following parameters are data-loaded here:
C     RHCUT  = factor by which H is reduced on retry of Newton solve.
C     RATEMX = maximum convergence rate for which Newton iteration
C              is considered converging.
C
      SAVE RHCUT, RATEMX
      DATA RHCUT/0.1D0/, RATEMX/0.8D0/
C
C
C-----------------------------------------------------------------------
C     BLOCK 1.
C     Initializations.
C     JSKIP is a flag set to 1 when NIC = 2 and NH = 1, to signal that
C     the initial call to the JAC routine is to be skipped then.
C     Save Y and YPRIME in PHI.  Initialize IDID, NH, and CJ.
C-----------------------------------------------------------------------
C
      MXNH = IWM(LMXNH)
      IDID = 1
      NH = 1
      JSKIP = 0
      IF (NIC .EQ. 2) JSKIP = 1
      CALL DCOPY (NEQ, Y, 1, PHI(1,1), 1)
      CALL DCOPY (NEQ, YPRIME, 1, PHI(1,2), 1)
C
      IF (ICOPT .EQ. 2) THEN
        CJ = 0.0D0
      ELSE
        CJ = 1.0D0/H
      ENDIF
C
C-----------------------------------------------------------------------
C     BLOCK 2
C     Call the nonlinear system solver to obtain
C     consistent initial values for Y and YPRIME.
C-----------------------------------------------------------------------
C
 200  CONTINUE
      CALL NLSIC(X,Y,YPRIME,NEQ,ICOPT,ID,RES,JAC,PSOL,H,WT,JSKIP,
     *   RPAR,IPAR,SAVR,DELTA,E,YIC,YPIC,PWK,WM,IWM,CJ,UROUND,
     *   EPLI,SQRTN,RSQRTN,EPCONI,RATEMX,STPTOL,JFLG,ICNFLG,ICNSTR,
     *   IERNLS)
C
      IF (IERNLS .EQ. 0) RETURN
C
C-----------------------------------------------------------------------
C     BLOCK 3
C     The nonlinear solver was unsuccessful.  Increment NCFN.
C     Return with IDID = -12 if either
C       IERNLS = -1: error is considered unrecoverable,
C       ICOPT = 2: we are doing initialization problem type 2, or
C       NH = MXNH: the maximum number of H values has been tried.
C     Otherwise (problem 1 with IERNLS .GE. 1), reduce H and try again.
C     If IERNLS > 1, restore Y and YPRIME to their original values.
C-----------------------------------------------------------------------
C
      IWM(LCFN) = IWM(LCFN) + 1
      JSKIP = 0
C
      IF (IERNLS .EQ. -1) GO TO 350
      IF (ICOPT .EQ. 2) GO TO 350
      IF (NH .EQ. MXNH) GO TO 350
C
      NH = NH + 1
      H = H*RHCUT
      CJ = 1.0D0/H
C
      IF (IERNLS .EQ. 1) GO TO 200
C
      CALL DCOPY (NEQ, PHI(1,1), 1, Y, 1)
      CALL DCOPY (NEQ, PHI(1,2), 1, YPRIME, 1)
      GO TO 200
C
 350  IDID = -12
      RETURN
C
C------END OF SUBROUTINE DDASIC-----------------------------------------
      END

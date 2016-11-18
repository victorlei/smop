C Work performed under the auspices of the U.S. Department of Energy
C by Lawrence Livermore National Laboratory under contract number
C W-7405-Eng-48.
C
      SUBROUTINE DNSD(X,Y,YPRIME,NEQ,RES,PDUM,WT,RPAR,IPAR,
     *   DUMSVR,DELTA,E,WM,IWM,CJ,DUMS,DUMR,DUME,EPCON,
     *   S,CONFAC,TOLNEW,MULDEL,MAXIT,IRES,IDUM,IERNEW)
C
C***BEGIN PROLOGUE  DNSD
C***REFER TO  DDASPK
C***DATE WRITTEN   891219   (YYMMDD)
C***REVISION DATE  900926   (YYMMDD)
C***REVISION DATE  950126   (YYMMDD)
C
C
C-----------------------------------------------------------------------
C***DESCRIPTION
C
C     DNSD solves a nonlinear system of
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
C     PDUM      -- Dummy argument.
C     WT        -- Vector of weights for error criterion.
C     RPAR,IPAR -- Real and integer arrays used for communication
C                  between the calling program and external user
C                  routines.  They are not altered within DASPK.
C     DUMSVR    -- Dummy argument.
C     DELTA     -- Work vector for DNSD of length NEQ.
C     E         -- Error accumulation vector for DNSD of length NEQ.
C     WM,IWM    -- Real and integer arrays storing
C                  matrix information such as the matrix
C                  of partial derivatives, permutation
C                  vector, and various other information.
C     CJ        -- Parameter always proportional to 1/H (step size).
C     DUMS      -- Dummy argument.
C     DUMR      -- Dummy argument.
C     DUME      -- Dummy argument.
C     EPCON     -- Tolerance to test for convergence of the Newton
C                  iteration.
C     S         -- Used for error convergence tests.
C                  In the Newton iteration: S = RATE/(1 - RATE),
C                  where RATE is the estimated rate of convergence
C                  of the Newton iteration.
C                  The calling routine passes the initial value
C                  of S to the Newton iteration.
C     CONFAC    -- A residual scale factor to improve convergence.
C     TOLNEW    -- Tolerance on the norm of Newton correction in
C                  alternative Newton convergence test.
C     MULDEL    -- A flag indicating whether or not to multiply
C                  DELTA by CONFAC.
C                  0  ==> do not scale DELTA by CONFAC.
C                  1  ==> scale DELTA by CONFAC.
C     MAXIT     -- Maximum allowed number of Newton iterations.
C     IRES      -- Error flag returned from RES.  See RES description
C                  in DDASPK prologue.  If IRES = -1, then IERNEW
C                  will be set to 1.
C                  If IRES < -1, then IERNEW will be set to -1.
C     IDUM      -- Dummy argument.
C     IERNEW    -- Error flag for Newton iteration.
C                   0  ==> Newton iteration converged.
C                   1  ==> recoverable error inside Newton iteration.
C                  -1  ==> unrecoverable error inside Newton iteration.
C
C     All arguments with "DUM" in their names are dummy arguments
C     which are not used in this routine.
C-----------------------------------------------------------------------
C
C***ROUTINES CALLED
C   DSLVD, DDWNRM, RES
C
C***END PROLOGUE  DNSD
C
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      DIMENSION Y(*),YPRIME(*),WT(*),DELTA(*),E(*)
      DIMENSION WM(*),IWM(*), RPAR(*),IPAR(*)
      EXTERNAL  RES
C
      PARAMETER (LNRE=12, LNNI=19)
C
C     Initialize Newton counter M and accumulation vector E.
C
      M = 0
      DO 100 I=1,NEQ
100     E(I)=0.0D0
C
C     Corrector loop.
C
300   CONTINUE
      IWM(LNNI) = IWM(LNNI) + 1
C
C     If necessary, multiply residual by convergence factor.
C
      IF (MULDEL .EQ. 1) THEN
         DO 320 I = 1,NEQ
320        DELTA(I) = DELTA(I) * CONFAC
        ENDIF
C
C     Compute a new iterate (back-substitution).
C     Store the correction in DELTA.
C
      CALL DSLVD(NEQ,DELTA,WM,IWM)
C
C     Update Y, E, and YPRIME.
C
      DO 340 I=1,NEQ
         Y(I)=Y(I)-DELTA(I)
         E(I)=E(I)-DELTA(I)
340      YPRIME(I)=YPRIME(I)-CJ*DELTA(I)
C
C     Test for convergence of the iteration.
C
      DELNRM=DDWNRM(NEQ,DELTA,WT,RPAR,IPAR)
      IF (DELNRM .LE. TOLNEW) GO TO 370
      IF (M .EQ. 0) THEN
        OLDNRM = DELNRM
      ELSE
        RATE = (DELNRM/OLDNRM)**(1.0D0/M)
        IF (RATE .GT. 0.9D0) GO TO 380
        S = RATE/(1.0D0 - RATE)
      ENDIF
      IF (S*DELNRM .LE. EPCON) GO TO 370
C
C     The corrector has not yet converged.
C     Update M and test whether the
C     maximum number of iterations have
C     been tried.
C
      M=M+1
      IF(M.GE.MAXIT) GO TO 380
C
C     Evaluate the residual,
C     and go back to do another iteration.
C
      IWM(LNRE)=IWM(LNRE)+1
      CALL RES(X,Y,YPRIME,CJ,DELTA,IRES,RPAR,IPAR)
      IF (IRES .LT. 0) GO TO 380
      GO TO 300
C
C     The iteration has converged.
C
370   RETURN
C
C     The iteration has not converged.  Set IERNEW appropriately.
C
380   CONTINUE
      IF (IRES .LE. -2 ) THEN
         IERNEW = -1
      ELSE
         IERNEW = 1
      ENDIF
      RETURN
C
C
C------END OF SUBROUTINE DNSD-------------------------------------------
      END

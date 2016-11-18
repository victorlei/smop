C Work performed under the auspices of the U.S. Department of Energy
C by Lawrence Livermore National Laboratory under contract number
C W-7405-Eng-48.
C
      SUBROUTINE DDSTP(X,Y,YPRIME,NEQ,RES,JAC,PSOL,H,WT,VT,
     *  JSTART,IDID,RPAR,IPAR,PHI,SAVR,DELTA,E,WM,IWM,
     *  ALPHA,BETA,GAMMA,PSI,SIGMA,CJ,CJOLD,HOLD,S,HMIN,UROUND,
     *  EPLI,SQRTN,RSQRTN,EPCON,IPHASE,JCALC,JFLG,K,KOLD,NS,NONNEG,
     *  NTYPE,NLS)
C
C***BEGIN PROLOGUE  DDSTP
C***REFER TO  DDASPK
C***DATE WRITTEN   890101   (YYMMDD)
C***REVISION DATE  900926   (YYMMDD)
C***REVISION DATE  940909   (YYMMDD) (Reset PSI(1), PHI(*,2) at 690)
C
C
C-----------------------------------------------------------------------
C***DESCRIPTION
C
C     DDSTP solves a system of differential/algebraic equations of
C     the form G(X,Y,YPRIME) = 0, for one step (normally from X to X+H).
C
C     The methods used are modified divided difference, fixed leading
C     coefficient forms of backward differentiation formulas.
C     The code adjusts the stepsize and order to control the local error
C     per step.
C
C
C     The parameters represent
C     X  --        Independent variable.
C     Y  --        Solution vector at X.
C     YPRIME --    Derivative of solution vector
C                  after successful step.
C     NEQ --       Number of equations to be integrated.
C     RES --       External user-supplied subroutine
C                  to evaluate the residual.  See RES description
C                  in DDASPK prologue.
C     JAC --       External user-supplied routine to update
C                  Jacobian or preconditioner information in the
C                  nonlinear solver.  See JAC description in DDASPK
C                  prologue.
C     PSOL --      External user-supplied routine to solve
C                  a linear system using preconditioning.
C                  (This is optional).  See PSOL in DDASPK prologue.
C     H --         Appropriate step size for next step.
C                  Normally determined by the code.
C     WT --        Vector of weights for error criterion used in Newton test.
C     VT --        Masked vector of weights used in error test.
C     JSTART --    Integer variable set 0 for
C                  first step, 1 otherwise.
C     IDID --      Completion code returned from the nonlinear solver.
C                  See IDID description in DDASPK prologue.
C     RPAR,IPAR -- Real and integer parameter arrays that
C                  are used for communication between the
C                  calling program and external user routines.
C                  They are not altered by DNSK
C     PHI --       Array of divided differences used by
C                  DDSTP. The length is NEQ*(K+1), where
C                  K is the maximum order.
C     SAVR --      Work vector for DDSTP of length NEQ.
C     DELTA,E --   Work vectors for DDSTP of length NEQ.
C     WM,IWM --    Real and integer arrays storing
C                  information required by the linear solver.
C
C     The other parameters are information
C     which is needed internally by DDSTP to
C     continue from step to step.
C
C-----------------------------------------------------------------------
C***ROUTINES CALLED
C   NLS, DDWNRM, DDATRP
C
C***END PROLOGUE  DDSTP
C
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      DIMENSION Y(*),YPRIME(*),WT(*),VT(*)
      DIMENSION PHI(NEQ,*),SAVR(*),DELTA(*),E(*)
      DIMENSION WM(*),IWM(*)
      DIMENSION PSI(*),ALPHA(*),BETA(*),GAMMA(*),SIGMA(*)
      DIMENSION RPAR(*),IPAR(*)
      EXTERNAL  RES, JAC, PSOL, NLS
C
      PARAMETER (LMXORD=3)
      PARAMETER (LNST=11, LETF=14, LCFN=15)
C
C
C-----------------------------------------------------------------------
C     BLOCK 1.
C     Initialize.  On the first call, set
C     the order to 1 and initialize
C     other variables.
C-----------------------------------------------------------------------
C
C     Initializations for all calls
C
      XOLD=X
      NCF=0
      NEF=0
      IF(JSTART .NE. 0) GO TO 120
C
C     If this is the first step, perform
C     other initializations
C
      K=1
      KOLD=0
      HOLD=0.0D0
      PSI(1)=H
      CJ = 1.D0/H
      IPHASE = 0
      NS=0
120   CONTINUE
C
C
C
C
C
C-----------------------------------------------------------------------
C     BLOCK 2
C     Compute coefficients of formulas for
C     this step.
C-----------------------------------------------------------------------
200   CONTINUE
      KP1=K+1
      KP2=K+2
      KM1=K-1
      IF(H.NE.HOLD.OR.K .NE. KOLD) NS = 0
      NS=MIN0(NS+1,KOLD+2)
      NSP1=NS+1
      IF(KP1 .LT. NS)GO TO 230
C
      BETA(1)=1.0D0
      ALPHA(1)=1.0D0
      TEMP1=H
      GAMMA(1)=0.0D0
      SIGMA(1)=1.0D0
      DO 210 I=2,KP1
         TEMP2=PSI(I-1)
         PSI(I-1)=TEMP1
         BETA(I)=BETA(I-1)*PSI(I-1)/TEMP2
         TEMP1=TEMP2+H
         ALPHA(I)=H/TEMP1
         SIGMA(I)=(I-1)*SIGMA(I-1)*ALPHA(I)
         GAMMA(I)=GAMMA(I-1)+ALPHA(I-1)/H
210      CONTINUE
      PSI(KP1)=TEMP1
230   CONTINUE
C
C     Compute ALPHAS, ALPHA0
C
      ALPHAS = 0.0D0
      ALPHA0 = 0.0D0
      DO 240 I = 1,K
        ALPHAS = ALPHAS - 1.0D0/I
        ALPHA0 = ALPHA0 - ALPHA(I)
240     CONTINUE
C
C     Compute leading coefficient CJ
C
      CJLAST = CJ
      CJ = -ALPHAS/H
C
C     Compute variable stepsize error coefficient CK
C
      CK = ABS(ALPHA(KP1) + ALPHAS - ALPHA0)
      CK = MAX(CK,ALPHA(KP1))
C
C     Change PHI to PHI STAR
C
      IF(KP1 .LT. NSP1) GO TO 280
      DO 270 J=NSP1,KP1
         DO 260 I=1,NEQ
260         PHI(I,J)=BETA(J)*PHI(I,J)
270      CONTINUE
280   CONTINUE
C
C     Update time
C
      X=X+H
C
C     Initialize IDID to 1
C
      IDID = 1
C
C
C
C
C
C-----------------------------------------------------------------------
C     BLOCK 3
C     Call the nonlinear system solver to obtain the solution and
C     derivative.
C-----------------------------------------------------------------------
C
      CALL NLS(X,Y,YPRIME,NEQ,
     *   RES,JAC,PSOL,H,WT,JSTART,IDID,RPAR,IPAR,PHI,GAMMA,
     *   SAVR,DELTA,E,WM,IWM,CJ,CJOLD,CJLAST,S,
     *   UROUND,EPLI,SQRTN,RSQRTN,EPCON,JCALC,JFLG,KP1,
     *   NONNEG,NTYPE,IERNLS)
C
      IF(IERNLS .NE. 0)GO TO 600
C
C
C
C
C
C-----------------------------------------------------------------------
C     BLOCK 4
C     Estimate the errors at orders K,K-1,K-2
C     as if constant stepsize was used. Estimate
C     the local error at order K and test
C     whether the current step is successful.
C-----------------------------------------------------------------------
C
C     Estimate errors at orders K,K-1,K-2
C
      ENORM = DDWNRM(NEQ,E,VT,RPAR,IPAR)
      ERK = SIGMA(K+1)*ENORM
      TERK = (K+1)*ERK
      EST = ERK
      KNEW=K
      IF(K .EQ. 1)GO TO 430
      DO 405 I = 1,NEQ
405     DELTA(I) = PHI(I,KP1) + E(I)
      ERKM1=SIGMA(K)*DDWNRM(NEQ,DELTA,VT,RPAR,IPAR)
      TERKM1 = K*ERKM1
      IF(K .GT. 2)GO TO 410
      IF(TERKM1 .LE. 0.5*TERK)GO TO 420
      GO TO 430
410   CONTINUE
      DO 415 I = 1,NEQ
415     DELTA(I) = PHI(I,K) + DELTA(I)
      ERKM2=SIGMA(K-1)*DDWNRM(NEQ,DELTA,VT,RPAR,IPAR)
      TERKM2 = (K-1)*ERKM2
      IF(MAX(TERKM1,TERKM2).GT.TERK)GO TO 430
C
C     Lower the order
C
420   CONTINUE
      KNEW=K-1
      EST = ERKM1
C
C
C     Calculate the local error for the current step
C     to see if the step was successful
C
430   CONTINUE
      ERR = CK * ENORM
      IF(ERR .GT. 1.0D0)GO TO 600
C
C
C
C
C
C-----------------------------------------------------------------------
C     BLOCK 5
C     The step is successful. Determine
C     the best order and stepsize for
C     the next step. Update the differences
C     for the next step.
C-----------------------------------------------------------------------
      IDID=1
      IWM(LNST)=IWM(LNST)+1
      KDIFF=K-KOLD
      KOLD=K
      HOLD=H
C
C
C     Estimate the error at order K+1 unless
C        already decided to lower order, or
C        already using maximum order, or
C        stepsize not constant, or
C        order raised in previous step
C
      IF(KNEW.EQ.KM1.OR.K.EQ.IWM(LMXORD))IPHASE=1
      IF(IPHASE .EQ. 0)GO TO 545
      IF(KNEW.EQ.KM1)GO TO 540
      IF(K.EQ.IWM(LMXORD)) GO TO 550
      IF(KP1.GE.NS.OR.KDIFF.EQ.1)GO TO 550
      DO 510 I=1,NEQ
510      DELTA(I)=E(I)-PHI(I,KP2)
      ERKP1 = (1.0D0/(K+2))*DDWNRM(NEQ,DELTA,VT,RPAR,IPAR)
      TERKP1 = (K+2)*ERKP1
      IF(K.GT.1)GO TO 520
      IF(TERKP1.GE.0.5D0*TERK)GO TO 550
      GO TO 530
520   IF(TERKM1.LE.MIN(TERK,TERKP1))GO TO 540
      IF(TERKP1.GE.TERK.OR.K.EQ.IWM(LMXORD))GO TO 550
C
C     Raise order
C
530   K=KP1
      EST = ERKP1
      GO TO 550
C
C     Lower order
C
540   K=KM1
      EST = ERKM1
      GO TO 550
C
C     If IPHASE = 0, increase order by one and multiply stepsize by
C     factor two
C
545   K = KP1
      HNEW = H*2.0D0
      H = HNEW
      GO TO 575
C
C
C     Determine the appropriate stepsize for
C     the next step.
C
550   HNEW=H
      TEMP2=K+1
      R=(2.0D0*EST+0.0001D0)**(-1.0D0/TEMP2)
      IF(R .LT. 2.0D0) GO TO 555
      HNEW = 2.0D0*H
      GO TO 560
555   IF(R .GT. 1.0D0) GO TO 560
      R = MAX(0.5D0,MIN(0.9D0,R))
      HNEW = H*R
560   H=HNEW
C
C
C     Update differences for next step
C
575   CONTINUE
      IF(KOLD.EQ.IWM(LMXORD))GO TO 585
      DO 580 I=1,NEQ
580      PHI(I,KP2)=E(I)
585   CONTINUE
      DO 590 I=1,NEQ
590      PHI(I,KP1)=PHI(I,KP1)+E(I)
      DO 595 J1=2,KP1
         J=KP1-J1+1
         DO 595 I=1,NEQ
595      PHI(I,J)=PHI(I,J)+PHI(I,J+1)
      JSTART = 1
      RETURN
C
C
C
C
C
C-----------------------------------------------------------------------
C     BLOCK 6
C     The step is unsuccessful. Restore X,PSI,PHI
C     Determine appropriate stepsize for
C     continuing the integration, or exit with
C     an error flag if there have been many
C     failures.
C-----------------------------------------------------------------------
600   IPHASE = 1
C
C     Restore X,PHI,PSI
C
      X=XOLD
      IF(KP1.LT.NSP1)GO TO 630
      DO 620 J=NSP1,KP1
         TEMP1=1.0D0/BETA(J)
         DO 610 I=1,NEQ
610         PHI(I,J)=TEMP1*PHI(I,J)
620      CONTINUE
630   CONTINUE
      DO 640 I=2,KP1
640      PSI(I-1)=PSI(I)-H
C
C
C     Test whether failure is due to nonlinear solver
C     or error test
C
      IF(IERNLS .EQ. 0)GO TO 660
      IWM(LCFN)=IWM(LCFN)+1
C
C
C     The nonlinear solver failed to converge.
C     Determine the cause of the failure and take appropriate action.
C     If IERNLS .LT. 0, then return.  Otherwise, reduce the stepsize
C     and try again, unless too many failures have occurred.
C
      IF (IERNLS .LT. 0) GO TO 675
      NCF = NCF + 1
      R = 0.25D0
      H = H*R
      IF (NCF .LT. 10 .AND. ABS(H) .GE. HMIN) GO TO 690
      IF (IDID .EQ. 1) IDID = -7
      IF (NEF .GE. 3) IDID = -9
      GO TO 675
C
C
C     The nonlinear solver converged, and the cause
C     of the failure was the error estimate
C     exceeding the tolerance.
C
660   NEF=NEF+1
      IWM(LETF)=IWM(LETF)+1
      IF (NEF .GT. 1) GO TO 665
C
C     On first error test failure, keep current order or lower
C     order by one.  Compute new stepsize based on differences
C     of the solution.
C
      K = KNEW
      TEMP2 = K + 1
      R = 0.90D0*(2.0D0*EST+0.0001D0)**(-1.0D0/TEMP2)
      R = MAX(0.25D0,MIN(0.9D0,R))
      H = H*R
      IF (ABS(H) .GE. HMIN) GO TO 690
      IDID = -6
      GO TO 675
C
C     On second error test failure, use the current order or
C     decrease order by one.  Reduce the stepsize by a factor of
C     one quarter.
C
665   IF (NEF .GT. 2) GO TO 670
      K = KNEW
      R = 0.25D0
      H = R*H
      IF (ABS(H) .GE. HMIN) GO TO 690
      IDID = -6
      GO TO 675
C
C     On third and subsequent error test failures, set the order to
C     one, and reduce the stepsize by a factor of one quarter.
C
670   K = 1
      R = 0.25D0
      H = R*H
      IF (ABS(H) .GE. HMIN) GO TO 690
      IDID = -6
      GO TO 675
C
C
C
C
C     For all crashes, restore Y to its last value,
C     interpolate to find YPRIME at last X, and return.
C
C     Before returning, verify that the user has not set
C     IDID to a nonnegative value.  If the user has set IDID
C     to a nonnegative value, then reset IDID to be -7, indicating
C     a failure in the nonlinear system solver.
C
675   CONTINUE
      CALL DDATRP(X,X,Y,YPRIME,NEQ,K,PHI,PSI)
      JSTART = 1
      IF (IDID .GE. 0) IDID = -7
      RETURN
C
C
C     Go back and try this step again.
C     If this is the first step, reset PSI(1) and rescale PHI(*,2).
C
690   IF (KOLD .EQ. 0) THEN
        PSI(1) = H
        DO 695 I = 1,NEQ
695       PHI(I,2) = R*PHI(I,2)
        ENDIF
      GO TO 200
C
C------END OF SUBROUTINE DDSTP------------------------------------------
      END

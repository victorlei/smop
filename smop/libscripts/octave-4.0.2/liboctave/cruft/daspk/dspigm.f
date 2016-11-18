C Work performed under the auspices of the U.S. Department of Energy
C by Lawrence Livermore National Laboratory under contract number
C W-7405-Eng-48.
C
      SUBROUTINE DSPIGM (NEQ, TN, Y, YPRIME, SAVR, R, WGHT, MAXL,
     *   MAXLP1, KMP, EPLIN, CJ, RES, IRES, NRE, PSOL, NPSL, Z, V,
     *   HES, Q, LGMR, WP, IWP, WK, DL, RHOK, IFLAG, IRST, NRSTS,
     *   RPAR, IPAR)
C
C***BEGIN PROLOGUE  DSPIGM
C***DATE WRITTEN   890101   (YYMMDD)
C***REVISION DATE  900926   (YYMMDD)
C***REVISION DATE  940927   Removed MNEWT and added RHOK in call list.
C
C
C-----------------------------------------------------------------------
C***DESCRIPTION
C
C This routine solves the linear system A * Z = R using a scaled
C preconditioned version of the generalized minimum residual method.
C An initial guess of Z = 0 is assumed.
C
C      On entry
C
C          NEQ = Problem size, passed to PSOL.
C
C           TN = Current Value of T.
C
C            Y = Array Containing current dependent variable vector.
C
C       YPRIME = Array Containing current first derivative of Y.
C
C         SAVR = Array containing current value of G(T,Y,YPRIME).
C
C            R = The right hand side of the system A*Z = R.
C                R is also used as work space when computing
C                the final approximation and will therefore be
C                destroyed.
C                (R is the same as V(*,MAXL+1) in the call to DSPIGM.)
C
C         WGHT = The vector of length NEQ containing the nonzero
C                elements of the diagonal scaling matrix.
C
C         MAXL = The maximum allowable order of the matrix H.
C
C       MAXLP1 = MAXL + 1, used for dynamic dimensioning of HES.
C
C          KMP = The number of previous vectors the new vector, VNEW,
C                must be made orthogonal to.  (KMP .LE. MAXL.)
C
C        EPLIN = Tolerance on residuals R-A*Z in weighted rms norm.
C
C           CJ = Scalar proportional to current value of
C                1/(step size H).
C
C           WK = Real work array used by routine DATV and PSOL.
C
C           DL = Real work array used for calculation of the residual
C                norm RHO when the method is incomplete (KMP.LT.MAXL)
C                and/or when using restarting.
C
C           WP = Real work array used by preconditioner PSOL.
C
C          IWP = Integer work array used by preconditioner PSOL.
C
C         IRST = Method flag indicating if restarting is being
C                performed.  IRST .GT. 0 means restarting is active,
C                while IRST = 0 means restarting is not being used.
C
C        NRSTS = Counter for the number of restarts on the current
C                call to DSPIGM.  If NRSTS .GT. 0, then the residual
C                R is already scaled, and so scaling of R is not
C                necessary.
C
C
C      On Return
C
C         Z    = The final computed approximation to the solution
C                of the system A*Z = R.
C
C         LGMR = The number of iterations performed and
C                the current order of the upper Hessenberg
C                matrix HES.
C
C         NRE  = The number of calls to RES (i.e. DATV)
C
C         NPSL = The number of calls to PSOL.
C
C         V    = The neq by (LGMR+1) array containing the LGMR
C                orthogonal vectors V(*,1) to V(*,LGMR).
C
C         HES  = The upper triangular factor of the QR decomposition
C                of the (LGMR+1) by LGMR upper Hessenberg matrix whose
C                entries are the scaled inner-products of A*V(*,I)
C                and V(*,K).
C
C         Q    = Real array of length 2*MAXL containing the components
C                of the givens rotations used in the QR decomposition
C                of HES.  It is loaded in DHEQR and used in DHELS.
C
C         IRES = Error flag from RES.
C
C           DL = Scaled preconditioned residual,
C                (D-inverse)*(P-inverse)*(R-A*Z). Only loaded when
C                performing restarts of the Krylov iteration.
C
C         RHOK = Weighted norm of final preconditioned residual.
C
C        IFLAG = Integer error flag..
C                0 Means convergence in LGMR iterations, LGMR.LE.MAXL.
C                1 Means the convergence test did not pass in MAXL
C                  iterations, but the new residual norm (RHO) is
C                  .LT. the old residual norm (RNRM), and so Z is
C                  computed.
C                2 Means the convergence test did not pass in MAXL
C                  iterations, new residual norm (RHO) .GE. old residual
C                  norm (RNRM), and the initial guess, Z = 0, is
C                  returned.
C                3 Means there was a recoverable error in PSOL
C                  caused by the preconditioner being out of date.
C               -1 Means there was an unrecoverable error in PSOL.
C
C-----------------------------------------------------------------------
C***ROUTINES CALLED
C   PSOL, DNRM2, DSCAL, DATV, DORTH, DHEQR, DCOPY, DHELS, DAXPY
C
C***END PROLOGUE  DSPIGM
C
      INTEGER NEQ,MAXL,MAXLP1,KMP,IRES,NRE,NPSL,LGMR,IWP,
     1   IFLAG,IRST,NRSTS,IPAR
      DOUBLE PRECISION TN,Y,YPRIME,SAVR,R,WGHT,EPLIN,CJ,Z,V,HES,Q,WP,WK,
     1   DL,RHOK,RPAR
      DIMENSION Y(*), YPRIME(*), SAVR(*), R(*), WGHT(*), Z(*),
     1   V(NEQ,*), HES(MAXLP1,*), Q(*), WP(*), IWP(*), WK(*), DL(*),
     2   RPAR(*), IPAR(*)
      INTEGER I, IER, INFO, IP1, I2, J, K, LL, LLP1
      DOUBLE PRECISION RNRM,C,DLNRM,PROD,RHO,S,SNORMW,DNRM2,TEM
      EXTERNAL  RES, PSOL
C
      IER = 0
      IFLAG = 0
      LGMR = 0
      NPSL = 0
      NRE = 0
C-----------------------------------------------------------------------
C The initial guess for Z is 0.  The initial residual is therefore
C the vector R.  Initialize Z to 0.
C-----------------------------------------------------------------------
      DO 10 I = 1,NEQ
 10     Z(I) = 0.0D0
C-----------------------------------------------------------------------
C Apply inverse of left preconditioner to vector R if NRSTS .EQ. 0.
C Form V(*,1), the scaled preconditioned right hand side.
C-----------------------------------------------------------------------
      IF (NRSTS .EQ. 0) THEN
         CALL PSOL (NEQ, TN, Y, YPRIME, SAVR, WK, CJ, WGHT, WP, IWP,
     1      R, EPLIN, IER, RPAR, IPAR)
         NPSL = 1
         IF (IER .NE. 0) GO TO 300
         DO 30 I = 1,NEQ
 30         V(I,1) = R(I)*WGHT(I)
      ELSE
         DO 35 I = 1,NEQ
 35         V(I,1) = R(I)
      ENDIF
C-----------------------------------------------------------------------
C Calculate norm of scaled vector V(*,1) and normalize it
C If, however, the norm of V(*,1) (i.e. the norm of the preconditioned
C residual) is .le. EPLIN, then return with Z=0.
C-----------------------------------------------------------------------
      RNRM = DNRM2 (NEQ, V, 1)
      IF (RNRM .LE. EPLIN) THEN
        RHOK = RNRM
        RETURN
        ENDIF
      TEM = 1.0D0/RNRM
      CALL DSCAL (NEQ, TEM, V(1,1), 1)
C-----------------------------------------------------------------------
C Zero out the HES array.
C-----------------------------------------------------------------------
      DO 65 J = 1,MAXL
        DO 60 I = 1,MAXLP1
 60       HES(I,J) = 0.0D0
 65     CONTINUE
C-----------------------------------------------------------------------
C Main loop to compute the vectors V(*,2) to V(*,MAXL).
C The running product PROD is needed for the convergence test.
C-----------------------------------------------------------------------
      PROD = 1.0D0
      DO 90 LL = 1,MAXL
        LGMR = LL
C-----------------------------------------------------------------------
C Call routine DATV to compute VNEW = ABAR*V(LL), where ABAR is
C the matrix A with scaling and inverse preconditioner factors applied.
C Call routine DORTH to orthogonalize the new vector VNEW = V(*,LL+1).
C call routine DHEQR to update the factors of HES.
C-----------------------------------------------------------------------
        CALL DATV (NEQ, Y, TN, YPRIME, SAVR, V(1,LL), WGHT, Z,
     1     RES, IRES, PSOL, V(1,LL+1), WK, WP, IWP, CJ, EPLIN,
     1     IER, NRE, NPSL, RPAR, IPAR)
        IF (IRES .LT. 0) RETURN
        IF (IER .NE. 0) GO TO 300
        CALL DORTH (V(1,LL+1), V, HES, NEQ, LL, MAXLP1, KMP, SNORMW)
        HES(LL+1,LL) = SNORMW
        CALL DHEQR (HES, MAXLP1, LL, Q, INFO, LL)
        IF (INFO .EQ. LL) GO TO 120
C-----------------------------------------------------------------------
C Update RHO, the estimate of the norm of the residual R - A*ZL.
C If KMP .LT. MAXL, then the vectors V(*,1),...,V(*,LL+1) are not
C necessarily orthogonal for LL .GT. KMP.  The vector DL must then
C be computed, and its norm used in the calculation of RHO.
C-----------------------------------------------------------------------
        PROD = PROD*Q(2*LL)
        RHO = ABS(PROD*RNRM)
        IF ((LL.GT.KMP) .AND. (KMP.LT.MAXL)) THEN
          IF (LL .EQ. KMP+1) THEN
            CALL DCOPY (NEQ, V(1,1), 1, DL, 1)
            DO 75 I = 1,KMP
              IP1 = I + 1
              I2 = I*2
              S = Q(I2)
              C = Q(I2-1)
              DO 70 K = 1,NEQ
 70             DL(K) = S*DL(K) + C*V(K,IP1)
 75           CONTINUE
            ENDIF
          S = Q(2*LL)
          C = Q(2*LL-1)/SNORMW
          LLP1 = LL + 1
          DO 80 K = 1,NEQ
 80         DL(K) = S*DL(K) + C*V(K,LLP1)
          DLNRM = DNRM2 (NEQ, DL, 1)
          RHO = RHO*DLNRM
          ENDIF
C-----------------------------------------------------------------------
C Test for convergence.  If passed, compute approximation ZL.
C If failed and LL .LT. MAXL, then continue iterating.
C-----------------------------------------------------------------------
        IF (RHO .LE. EPLIN) GO TO 200
        IF (LL .EQ. MAXL) GO TO 100
C-----------------------------------------------------------------------
C Rescale so that the norm of V(1,LL+1) is one.
C-----------------------------------------------------------------------
        TEM = 1.0D0/SNORMW
        CALL DSCAL (NEQ, TEM, V(1,LL+1), 1)
 90     CONTINUE
 100  CONTINUE
      IF (RHO .LT. RNRM) GO TO 150
 120  CONTINUE
      IFLAG = 2
      DO 130 I = 1,NEQ
 130     Z(I) = 0.D0
      RETURN
 150  IFLAG = 1
C-----------------------------------------------------------------------
C The tolerance was not met, but the residual norm was reduced.
C If performing restarting (IRST .gt. 0) calculate the residual vector
C RL and store it in the DL array.  If the incomplete version is
C being used (KMP .lt. MAXL) then DL has already been calculated.
C-----------------------------------------------------------------------
      IF (IRST .GT. 0) THEN
         IF (KMP .EQ. MAXL) THEN
C
C           Calculate DL from the V(I)'s.
C
            CALL DCOPY (NEQ, V(1,1), 1, DL, 1)
            MAXLM1 = MAXL - 1
            DO 175 I = 1,MAXLM1
               IP1 = I + 1
               I2 = I*2
               S = Q(I2)
               C = Q(I2-1)
               DO 170 K = 1,NEQ
 170              DL(K) = S*DL(K) + C*V(K,IP1)
 175        CONTINUE
            S = Q(2*MAXL)
            C = Q(2*MAXL-1)/SNORMW
            DO 180 K = 1,NEQ
 180           DL(K) = S*DL(K) + C*V(K,MAXLP1)
         ENDIF
C
C        Scale DL by RNRM*PROD to obtain the residual RL.
C
         TEM = RNRM*PROD
         CALL DSCAL(NEQ, TEM, DL, 1)
      ENDIF
C-----------------------------------------------------------------------
C Compute the approximation ZL to the solution.
C Since the vector Z was used as work space, and the initial guess
C of the Newton correction is zero, Z must be reset to zero.
C-----------------------------------------------------------------------
 200  CONTINUE
      LL = LGMR
      LLP1 = LL + 1
      DO 210 K = 1,LLP1
 210    R(K) = 0.0D0
      R(1) = RNRM
      CALL DHELS (HES, MAXLP1, LL, Q, R)
      DO 220 K = 1,NEQ
 220    Z(K) = 0.0D0
      DO 230 I = 1,LL
        CALL DAXPY (NEQ, R(I), V(1,I), 1, Z, 1)
 230    CONTINUE
      DO 240 I = 1,NEQ
 240    Z(I) = Z(I)/WGHT(I)
C Load RHO into RHOK.
      RHOK = RHO
      RETURN
C-----------------------------------------------------------------------
C This block handles error returns forced by routine PSOL.
C-----------------------------------------------------------------------
 300  CONTINUE
      IF (IER .LT. 0) IFLAG = -1
      IF (IER .GT. 0) IFLAG = 3
C
      RETURN
C
C------END OF SUBROUTINE DSPIGM-----------------------------------------
      END

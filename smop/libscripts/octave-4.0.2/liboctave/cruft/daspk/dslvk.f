C Work performed under the auspices of the U.S. Department of Energy
C by Lawrence Livermore National Laboratory under contract number
C W-7405-Eng-48.
C
      SUBROUTINE DSLVK (NEQ, Y, TN, YPRIME, SAVR, X, EWT, WM, IWM,
     *   RES, IRES, PSOL, IERSL, CJ, EPLIN, SQRTN, RSQRTN, RHOK,
     *   RPAR, IPAR)
C
C***BEGIN PROLOGUE  DSLVK
C***REFER TO  DDASPK
C***DATE WRITTEN   890101   (YYMMDD)
C***REVISION DATE  900926   (YYMMDD)
C***REVISION DATE  940928   Removed MNEWT and added RHOK in call list.
C
C
C-----------------------------------------------------------------------
C***DESCRIPTION
C
C DSLVK uses a restart algorithm and interfaces to DSPIGM for
C the solution of the linear system arising from a Newton iteration.
C
C In addition to variables described elsewhere,
C communication with DSLVK uses the following variables..
C WM    = Real work space containing data for the algorithm
C         (Krylov basis vectors, Hessenberg matrix, etc.).
C IWM   = Integer work space containing data for the algorithm.
C X     = The right-hand side vector on input, and the solution vector
C         on output, of length NEQ.
C IRES  = Error flag from RES.
C IERSL = Output flag ..
C         IERSL =  0 means no trouble occurred (or user RES routine
C                    returned IRES < 0)
C         IERSL =  1 means the iterative method failed to converge
C                    (DSPIGM returned IFLAG > 0.)
C         IERSL = -1 means there was a nonrecoverable error in the
C                    iterative solver, and an error exit will occur.
C-----------------------------------------------------------------------
C***ROUTINES CALLED
C   DSCAL, DCOPY, DSPIGM
C
C***END PROLOGUE  DSLVK
C
      INTEGER NEQ, IWM, IRES, IERSL, IPAR
      DOUBLE PRECISION Y, TN, YPRIME, SAVR, X, EWT, WM, CJ, EPLIN,
     1   SQRTN, RSQRTN, RHOK, RPAR
      DIMENSION Y(*), YPRIME(*), SAVR(*), X(*), EWT(*),
     1  WM(*), IWM(*), RPAR(*), IPAR(*)
C
      INTEGER IFLAG, IRST, NRSTS, NRMAX, LR, LDL, LHES, LGMR, LQ, LV,
     1        LWK, LZ, MAXLP1, NPSL
      INTEGER NLI, NPS, NCFL, NRE, MAXL, KMP, MITER
      EXTERNAL  RES, PSOL
C
      PARAMETER (LNRE=12, LNCFL=16, LNLI=20, LNPS=21)
      PARAMETER (LLOCWP=29, LLCIWP=30)
      PARAMETER (LMITER=23, LMAXL=24, LKMP=25, LNRMAX=26)
C
C-----------------------------------------------------------------------
C IRST is set to 1, to indicate restarting is in effect.
C NRMAX is the maximum number of restarts.
C-----------------------------------------------------------------------
      DATA IRST/1/
C
      LIWP = IWM(LLCIWP)
      NLI = IWM(LNLI)
      NPS = IWM(LNPS)
      NCFL = IWM(LNCFL)
      NRE = IWM(LNRE)
      LWP = IWM(LLOCWP)
      MAXL = IWM(LMAXL)
      KMP = IWM(LKMP)
      NRMAX = IWM(LNRMAX)
      MITER = IWM(LMITER)
      IERSL = 0
      IRES = 0
C-----------------------------------------------------------------------
C Use a restarting strategy to solve the linear system
C P*X = -F.  Parse the work vector, and perform initializations.
C Note that zero is the initial guess for X.
C-----------------------------------------------------------------------
      MAXLP1 = MAXL + 1
      LV = 1
      LR = LV + NEQ*MAXL
      LHES = LR + NEQ + 1
      LQ = LHES + MAXL*MAXLP1
      LWK = LQ + 2*MAXL
      LDL = LWK + MIN0(1,MAXL-KMP)*NEQ
      LZ = LDL + NEQ
      CALL DSCAL (NEQ, RSQRTN, EWT, 1)
      CALL DCOPY (NEQ, X, 1, WM(LR), 1)
      DO 110 I = 1,NEQ
 110     X(I) = 0.D0
C-----------------------------------------------------------------------
C Top of loop for the restart algorithm.  Initial pass approximates
C X and sets up a transformed system to perform subsequent restarts
C to update X.  NRSTS is initialized to -1, because restarting
C does not occur until after the first pass.
C Update NRSTS; conditionally copy DL to R; call the DSPIGM
C algorithm to solve A*Z = R;  updated counters;  update X with
C the residual solution.
C Note:  if convergence is not achieved after NRMAX restarts,
C then the linear solver is considered to have failed.
C-----------------------------------------------------------------------
      NRSTS = -1
 115  CONTINUE
      NRSTS = NRSTS + 1
      IF (NRSTS .GT. 0) CALL DCOPY (NEQ, WM(LDL), 1, WM(LR),1)
      CALL DSPIGM (NEQ, TN, Y, YPRIME, SAVR, WM(LR), EWT, MAXL, MAXLP1,
     1   KMP, EPLIN, CJ, RES, IRES, NRES, PSOL, NPSL, WM(LZ), WM(LV),
     2   WM(LHES), WM(LQ), LGMR, WM(LWP), IWM(LIWP), WM(LWK),
     3   WM(LDL), RHOK, IFLAG, IRST, NRSTS, RPAR, IPAR)
      NLI = NLI + LGMR
      NPS = NPS + NPSL
      NRE = NRE + NRES
      DO 120 I = 1,NEQ
 120     X(I) = X(I) + WM(LZ+I-1)
      IF ((IFLAG .EQ. 1) .AND. (NRSTS .LT. NRMAX) .AND. (IRES .EQ. 0))
     1   GO TO 115
C-----------------------------------------------------------------------
C The restart scheme is finished.  Test IRES and IFLAG to see if
C convergence was not achieved, and set flags accordingly.
C-----------------------------------------------------------------------
      IF (IRES .LT. 0) THEN
         NCFL = NCFL + 1
      ELSE IF (IFLAG .NE. 0) THEN
         NCFL = NCFL + 1
         IF (IFLAG .GT. 0) IERSL = 1
         IF (IFLAG .LT. 0) IERSL = -1
      ENDIF
C-----------------------------------------------------------------------
C Update IWM with counters, rescale EWT, and return.
C-----------------------------------------------------------------------
      IWM(LNLI)  = NLI
      IWM(LNPS)  = NPS
      IWM(LNCFL) = NCFL
      IWM(LNRE)  = NRE
      CALL DSCAL (NEQ, SQRTN, EWT, 1)
      RETURN
C
C------END OF SUBROUTINE DSLVK------------------------------------------
      END

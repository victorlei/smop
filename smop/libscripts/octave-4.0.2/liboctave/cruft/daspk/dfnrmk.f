C Work performed under the auspices of the U.S. Department of Energy
C by Lawrence Livermore National Laboratory under contract number
C W-7405-Eng-48.
C
      SUBROUTINE DFNRMK (NEQ, Y, T, YPRIME, SAVR, R, CJ, WT,
     *                   SQRTN, RSQRTN, RES, IRES, PSOL, IRIN, IER,
     *                   FNORM, EPLIN, WP, IWP, PWK, RPAR, IPAR)
C
C***BEGIN PROLOGUE  DFNRMK
C***REFER TO  DLINSK
C***DATE WRITTEN   940830   (YYMMDD)
C***REVISION DATE  951006   (SQRTN, RSQRTN, and scaling of WT added.)
C
C
C-----------------------------------------------------------------------
C***DESCRIPTION
C
C     DFNRMK calculates the scaled preconditioned norm of the nonlinear
C     function used in the nonlinear iteration for obtaining consistent
C     initial conditions.  Specifically, DFNRMK calculates the weighted
C     root-mean-square norm of the vector (P-inverse)*G(T,Y,YPRIME),
C     where P is the preconditioner matrix.
C
C     In addition to the parameters described in the calling program
C     DLINSK, the parameters represent
C
C     IRIN   -- Flag showing whether the current residual vector is
C               input in SAVR.  1 means it is, 0 means it is not.
C     R      -- Array of length NEQ that contains
C               (P-inverse)*G(T,Y,YPRIME) on return.
C     FNORM  -- Scalar containing the weighted norm of R on return.
C-----------------------------------------------------------------------
C
C***ROUTINES CALLED
C   RES, DCOPY, DSCAL, PSOL, DDWNRM
C
C***END PROLOGUE  DFNRMK
C
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      EXTERNAL RES, PSOL
      DIMENSION Y(*), YPRIME(*), WT(*), SAVR(*), R(*), PWK(*)
      DIMENSION WP(*), IWP(*), RPAR(*), IPAR(*)
C-----------------------------------------------------------------------
C     Call RES routine if IRIN = 0.
C-----------------------------------------------------------------------
      IF (IRIN .EQ. 0) THEN
        IRES = 0
        CALL RES (T, Y, YPRIME, CJ, SAVR, IRES, RPAR, IPAR)
        IF (IRES .LT. 0) RETURN
        ENDIF
C-----------------------------------------------------------------------
C     Apply inverse of left preconditioner to vector R.
C     First scale WT array by 1/sqrt(N), and undo scaling afterward.
C-----------------------------------------------------------------------
      CALL DCOPY(NEQ, SAVR, 1, R, 1)
      CALL DSCAL (NEQ, RSQRTN, WT, 1)
      IER = 0
      CALL PSOL (NEQ, T, Y, YPRIME, SAVR, PWK, CJ, WT, WP, IWP,
     *           R, EPLIN, IER, RPAR, IPAR)
      CALL DSCAL (NEQ, SQRTN, WT, 1)
      IF (IER .NE. 0) RETURN
C-----------------------------------------------------------------------
C     Calculate norm of R.
C-----------------------------------------------------------------------
      FNORM = DDWNRM (NEQ, R, WT, RPAR, IPAR)
C
      RETURN
C----------------------- END OF SUBROUTINE DFNRMK ----------------------
      END

C Work performed under the auspices of the U.S. Department of Energy
C by Lawrence Livermore National Laboratory under contract number
C W-7405-Eng-48.
C
      SUBROUTINE DFNRMD (NEQ, Y, T, YPRIME, R, CJ, WT, RES, IRES,
     *                   FNORM, WM, IWM, RPAR, IPAR)
C
C***BEGIN PROLOGUE  DFNRMD
C***REFER TO  DLINSD
C***DATE WRITTEN   941025   (YYMMDD)
C
C
C-----------------------------------------------------------------------
C***DESCRIPTION
C
C     DFNRMD calculates the scaled preconditioned norm of the nonlinear
C     function used in the nonlinear iteration for obtaining consistent
C     initial conditions.  Specifically, DFNRMD calculates the weighted
C     root-mean-square norm of the vector (J-inverse)*G(T,Y,YPRIME),
C     where J is the Jacobian matrix.
C
C     In addition to the parameters described in the calling program
C     DLINSD, the parameters represent
C
C     R      -- Array of length NEQ that contains
C               (J-inverse)*G(T,Y,YPRIME) on return.
C     FNORM  -- Scalar containing the weighted norm of R on return.
C-----------------------------------------------------------------------
C
C***ROUTINES CALLED
C   RES, DSLVD, DDWNRM
C
C***END PROLOGUE  DFNRMD
C
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      EXTERNAL RES
      DIMENSION Y(*), YPRIME(*), WT(*), R(*)
      DIMENSION WM(*),IWM(*), RPAR(*),IPAR(*)
C-----------------------------------------------------------------------
C     Call RES routine.
C-----------------------------------------------------------------------
      IRES = 0
      CALL RES(T,Y,YPRIME,CJ,R,IRES,RPAR,IPAR)
      IF (IRES .LT. 0) RETURN
C-----------------------------------------------------------------------
C     Apply inverse of Jacobian to vector R.
C-----------------------------------------------------------------------
      CALL DSLVD(NEQ,R,WM,IWM)
C-----------------------------------------------------------------------
C     Calculate norm of R.
C-----------------------------------------------------------------------
      FNORM = DDWNRM(NEQ,R,WT,RPAR,IPAR)
C
      RETURN
C----------------------- END OF SUBROUTINE DFNRMD ----------------------
      END

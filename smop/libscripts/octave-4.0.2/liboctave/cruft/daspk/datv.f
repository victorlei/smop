C Work performed under the auspices of the U.S. Department of Energy
C by Lawrence Livermore National Laboratory under contract number
C W-7405-Eng-48.
C
      SUBROUTINE DATV (NEQ, Y, TN, YPRIME, SAVR, V, WGHT, YPTEM, RES,
     *   IRES, PSOL, Z, VTEM, WP, IWP, CJ, EPLIN, IER, NRE, NPSL,
     *   RPAR,IPAR)
C
C***BEGIN PROLOGUE  DATV
C***DATE WRITTEN   890101   (YYMMDD)
C***REVISION DATE  900926   (YYMMDD)
C
C
C-----------------------------------------------------------------------
C***DESCRIPTION
C
C This routine computes the product
C
C   Z = (D-inverse)*(P-inverse)*(dF/dY)*(D*V),
C
C where F(Y) = G(T, Y, CJ*(Y-A)), CJ is a scalar proportional to 1/H,
C and A involves the past history of Y.  The quantity CJ*(Y-A) is
C an approximation to the first derivative of Y and is stored
C in the array YPRIME.  Note that dF/dY = dG/dY + CJ*dG/dYPRIME.
C
C D is a diagonal scaling matrix, and P is the left preconditioning
C matrix.  V is assumed to have L2 norm equal to 1.
C The product is stored in Z and is computed by means of a
C difference quotient, a call to RES, and one call to PSOL.
C
C      On entry
C
C          NEQ = Problem size, passed to RES and PSOL.
C
C            Y = Array containing current dependent variable vector.
C
C       YPRIME = Array containing current first derivative of y.
C
C         SAVR = Array containing current value of G(T,Y,YPRIME).
C
C            V = Real array of length NEQ (can be the same array as Z).
C
C         WGHT = Array of length NEQ containing scale factors.
C                1/WGHT(I) are the diagonal elements of the matrix D.
C
C        YPTEM = Work array of length NEQ.
C
C         VTEM = Work array of length NEQ used to store the
C                unscaled version of V.
C
C         WP = Real work array used by preconditioner PSOL.
C
C         IWP = Integer work array used by preconditioner PSOL.
C
C           CJ = Scalar proportional to current value of
C                1/(step size H).
C
C
C      On return
C
C            Z = Array of length NEQ containing desired scaled
C                matrix-vector product.
C
C         IRES = Error flag from RES.
C
C          IER = Error flag from PSOL.
C
C         NRE  = The number of calls to RES.
C
C         NPSL = The number of calls to PSOL.
C
C-----------------------------------------------------------------------
C***ROUTINES CALLED
C   RES, PSOL
C
C***END PROLOGUE  DATV
C
      INTEGER NEQ, IRES, IWP, IER, NRE, NPSL, IPAR
      DOUBLE PRECISION Y, TN, YPRIME, SAVR, V, WGHT, YPTEM, Z, VTEM,
     1   WP, CJ, RPAR
      DIMENSION Y(*), YPRIME(*), SAVR(*), V(*), WGHT(*), YPTEM(*),
     1   Z(*), VTEM(*), WP(*), IWP(*), RPAR(*), IPAR(*)
      INTEGER I
      DOUBLE PRECISION EPLIN
      EXTERNAL  RES, PSOL
C
      IRES = 0
C-----------------------------------------------------------------------
C Set VTEM = D * V.
C-----------------------------------------------------------------------
      DO 10 I = 1,NEQ
 10     VTEM(I) = V(I)/WGHT(I)
      IER = 0
C-----------------------------------------------------------------------
C Store Y in Z and increment Z by VTEM.
C Store YPRIME in YPTEM and increment YPTEM by VTEM*CJ.
C-----------------------------------------------------------------------
      DO 20 I = 1,NEQ
        YPTEM(I) = YPRIME(I) + VTEM(I)*CJ
 20     Z(I) = Y(I) + VTEM(I)
C-----------------------------------------------------------------------
C Call RES with incremented Y, YPRIME arguments
C stored in Z, YPTEM.  VTEM is overwritten with new residual.
C-----------------------------------------------------------------------
      CONTINUE
      CALL RES(TN,Z,YPTEM,CJ,VTEM,IRES,RPAR,IPAR)
      NRE = NRE + 1
      IF (IRES .LT. 0) RETURN
C-----------------------------------------------------------------------
C Set Z = (dF/dY) * VBAR using difference quotient.
C (VBAR is old value of VTEM before calling RES)
C-----------------------------------------------------------------------
      DO 70 I = 1,NEQ
 70     Z(I) = VTEM(I) - SAVR(I)
C-----------------------------------------------------------------------
C Apply inverse of left preconditioner to Z.
C-----------------------------------------------------------------------
      CALL PSOL (NEQ, TN, Y, YPRIME, SAVR, YPTEM, CJ, WGHT, WP, IWP,
     1   Z, EPLIN, IER, RPAR, IPAR)
      NPSL = NPSL + 1
      IF (IER .NE. 0) RETURN
C-----------------------------------------------------------------------
C Apply D-inverse to Z and return.
C-----------------------------------------------------------------------
      DO 90 I = 1,NEQ
 90     Z(I) = Z(I)*WGHT(I)
      RETURN
C
C------END OF SUBROUTINE DATV-------------------------------------------
      END

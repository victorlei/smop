      REAL FUNCTION gennf(dfn,dfd,xnonc)

C**********************************************************************
C
C     REAL FUNCTION GENNF( DFN, DFD, XNONC )
C           GENerate random deviate from the Noncentral F distribution
C
C
C                              Function
C
C
C     Generates a random deviate from the  noncentral F (variance ratio)
C     distribution with DFN degrees of freedom in the numerator, and DFD
C     degrees of freedom in the denominator, and noncentrality parameter
C     XNONC.
C
C
C                              Arguments
C
C
C     DFN --> Numerator degrees of freedom
C             (Must be >= 1.0)
C                              REAL DFN
C      DFD --> Denominator degrees of freedom
C             (Must be positive)
C                              REAL DFD
C
C     XNONC --> Noncentrality parameter
C               (Must be nonnegative)
C                              REAL XNONC
C
C
C                              Method
C
C
C     Directly generates ratio of noncentral numerator chisquare variate
C     to central denominator chisquare variate.
C
C**********************************************************************
C     .. Scalar Arguments ..
      REAL dfd,dfn,xnonc
C     ..
C     .. Local Scalars ..
      REAL xden,xnum
      LOGICAL qcond
C     ..
C     .. External Functions ..
C     JJV changed the code to call SGAMMA and SNORM directly
C      REAL genchi,gennch
C      EXTERNAL genchi,gennch
      REAL sgamma,snorm
      EXTERNAL sgamma,snorm
C     ..
C     .. Executable Statements ..
C     JJV changed the argument checker to allow DFN = 1.0
C     JJV in the same way as GENNCH was changed.
      qcond = dfn .LT. 1.0 .OR. dfd .LE. 0.0 .OR. xnonc .LT. 0.0
      IF (.NOT. (qcond)) GO TO 10
      WRITE (*,*) 'In GENNF - Either (1) Numerator DF < 1.0 or'
      WRITE (*,*) '(2) Denominator DF <= 0.0 or '
      WRITE (*,*) '(3) Noncentrality parameter < 0.0'
      WRITE (*,*) 'DFN value: ',dfn,'DFD value: ',dfd,'XNONC value: ',
     +  xnonc

      CALL XSTOPX
     + ('Degrees of freedom or noncent param out of range in GENNF')

C      GENNF = ( GENNCH( DFN, XNONC ) / DFN ) / ( GENCHI( DFD ) / DFD )
C     JJV changed this to call SGAMMA and SNORM directly
C     xnum = gennch(dfn,xnonc)/dfn
 10   IF (dfn.GE.1.000001) GO TO 20
C     JJV case dfn = 1.0 - here I am treating dfn as exactly 1.0
      xnum = (snorm() + sqrt(xnonc))**2
      GO TO 30

C     JJV case dfn > 1.0
 20   xnum = (2.0*sgamma((dfn-1.0)/2.0) + (snorm()+sqrt(xnonc))**2)/dfn

C     xden = genchi(dfd)/dfd
 30   xden = 2.0*sgamma(dfd/2.0)/dfd

C     JJV changed constant so that it will not underflow at compile time
C     JJV while not slowing generator by using double precision or logs.
C      IF (.NOT. (xden.LE. (1.0E-38*xnum))) GO TO 40
      IF (.NOT. (xden.LE. (1.0E-37*xnum))) GO TO 40
      WRITE (*,*) ' GENNF - generated numbers would cause overflow'
      WRITE (*,*) ' Numerator ',xnum,' Denominator ',xden
C     JJV next 2 lines changed to maintain truncation of large deviates.
C      WRITE (*,*) ' GENNF returning 1.0E38'
C      gennf = 1.0E38
      WRITE (*,*) ' GENNF returning 1.0E37'
      gennf = 1.0E37
      GO TO 50

   40 gennf = xnum/xden
   50 RETURN

      END

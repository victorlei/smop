*DECK PCHIM
      SUBROUTINE PCHIM (N, X, F, D, INCFD, IERR)
C***BEGIN PROLOGUE  PCHIM
C***PURPOSE  Set derivatives needed to determine a monotone piecewise
C            cubic Hermite interpolant to given data.  Boundary values
C            are provided which are compatible with monotonicity.  The
C            interpolant will have an extremum at each point where mono-
C            tonicity switches direction.  (See PCHIC if user control is
C            desired over boundary or switch conditions.)
C***LIBRARY   SLATEC (PCHIP)
C***CATEGORY  E1A
C***TYPE      SINGLE PRECISION (PCHIM-S, DPCHIM-D)
C***KEYWORDS  CUBIC HERMITE INTERPOLATION, MONOTONE INTERPOLATION,
C             PCHIP, PIECEWISE CUBIC INTERPOLATION
C***AUTHOR  Fritsch, F. N., (LLNL)
C             Lawrence Livermore National Laboratory
C             P.O. Box 808  (L-316)
C             Livermore, CA  94550
C             FTS 532-4275, (510) 422-4275
C***DESCRIPTION
C
C          PCHIM:  Piecewise Cubic Hermite Interpolation to
C                  Monotone data.
C
C     Sets derivatives needed to determine a monotone piecewise cubic
C     Hermite interpolant to the data given in X and F.
C
C     Default boundary conditions are provided which are compatible
C     with monotonicity.  (See PCHIC if user control of boundary con-
C     ditions is desired.)
C
C     If the data are only piecewise monotonic, the interpolant will
C     have an extremum at each point where monotonicity switches direc-
C     tion.  (See PCHIC if user control is desired in such cases.)
C
C     To facilitate two-dimensional applications, includes an increment
C     between successive values of the F- and D-arrays.
C
C     The resulting piecewise cubic Hermite function may be evaluated
C     by PCHFE or PCHFD.
C
C ----------------------------------------------------------------------
C
C  Calling sequence:
C
C        PARAMETER  (INCFD = ...)
C        INTEGER  N, IERR
C        REAL  X(N), F(INCFD,N), D(INCFD,N)
C
C        CALL  PCHIM (N, X, F, D, INCFD, IERR)
C
C   Parameters:
C
C     N -- (input) number of data points.  (Error return if N.LT.2 .)
C           If N=2, simply does linear interpolation.
C
C     X -- (input) real array of independent variable values.  The
C           elements of X must be strictly increasing:
C                X(I-1) .LT. X(I),  I = 2(1)N.
C           (Error return if not.)
C
C     F -- (input) real array of dependent variable values to be inter-
C           polated.  F(1+(I-1)*INCFD) is value corresponding to X(I).
C           PCHIM is designed for monotonic data, but it will work for
C           any F-array.  It will force extrema at points where mono-
C           tonicity switches direction.  If some other treatment of
C           switch points is desired, PCHIC should be used instead.
C                                     -----
C     D -- (output) real array of derivative values at the data points.
C           If the data are monotonic, these values will determine a
C           a monotone cubic Hermite function.
C           The value corresponding to X(I) is stored in
C                D(1+(I-1)*INCFD),  I=1(1)N.
C           No other entries in D are changed.
C
C     INCFD -- (input) increment between successive values in F and D.
C           This argument is provided primarily for 2-D applications.
C           (Error return if  INCFD.LT.1 .)
C
C     IERR -- (output) error flag.
C           Normal return:
C              IERR = 0  (no errors).
C           Warning error:
C              IERR.GT.0  means that IERR switches in the direction
C                 of monotonicity were detected.
C           "Recoverable" errors:
C              IERR = -1  if N.LT.2 .
C              IERR = -2  if INCFD.LT.1 .
C              IERR = -3  if the X-array is not strictly increasing.
C             (The D-array has not been changed in any of these cases.)
C               NOTE:  The above errors are checked in the order listed,
C                   and following arguments have **NOT** been validated.
C
C***REFERENCES  1. F. N. Fritsch and J. Butland, A method for construc-
C                 ting local monotone piecewise cubic interpolants, SIAM
C                 Journal on Scientific and Statistical Computing 5, 2
C                 (June 1984), pp. 300-304.
C               2. F. N. Fritsch and R. E. Carlson, Monotone piecewise
C                 cubic interpolation, SIAM Journal on Numerical Ana-
C                 lysis 17, 2 (April 1980), pp. 238-246.
C***ROUTINES CALLED  PCHST, XERMSG
C***REVISION HISTORY  (YYMMDD)
C   811103  DATE WRITTEN
C   820201  1. Introduced  PCHST  to reduce possible over/under-
C             flow problems.
C           2. Rearranged derivative formula for same reason.
C   820602  1. Modified end conditions to be continuous functions
C             of data when monotonicity switches in next interval.
C           2. Modified formulas so end conditions are less prone
C             of over/underflow problems.
C   820803  Minor cosmetic changes for release 1.
C   870813  Updated Reference 1.
C   890411  Added SAVE statements (Vers. 3.2).
C   890531  Changed all specific intrinsics to generic.  (WRB)
C   890703  Corrected category record.  (WRB)
C   890831  Modified array declarations.  (WRB)
C   890831  REVISION DATE from Version 3.2
C   891214  Prologue converted to Version 4.0 format.  (BAB)
C   900315  CALLs to XERROR changed to CALLs to XERMSG.  (THJ)
C   920429  Revised format and order of references.  (WRB,FNF)
C***END PROLOGUE  PCHIM
C  Programming notes:
C
C     1. The function  PCHST(ARG1,ARG2)  is assumed to return zero if
C        either argument is zero, +1 if they are of the same sign, and
C        -1 if they are of opposite sign.
C     2. To produce a double precision version, simply:
C        a. Change PCHIM to DPCHIM wherever it occurs,
C        b. Change PCHST to DPCHST wherever it occurs,
C        c. Change all references to the Fortran intrinsics to their
C           double precision equivalents,
C        d. Change the real declarations to double precision, and
C        e. Change the constants ZERO and THREE to double precision.
C
C  DECLARE ARGUMENTS.
C
      INTEGER  N, INCFD, IERR
      REAL  X(*), F(INCFD,*), D(INCFD,*)
C
C  DECLARE LOCAL VARIABLES.
C
      INTEGER  I, NLESS1
      REAL  DEL1, DEL2, DMAX, DMIN, DRAT1, DRAT2, DSAVE,
     *      H1, H2, HSUM, HSUMT3, THREE, W1, W2, ZERO
      SAVE ZERO, THREE
      REAL  PCHST
      DATA  ZERO /0./,  THREE /3./
C
C  VALIDITY-CHECK ARGUMENTS.
C
C***FIRST EXECUTABLE STATEMENT  PCHIM
      IF ( N.LT.2 )  GO TO 5001
      IF ( INCFD.LT.1 )  GO TO 5002
      DO 1  I = 2, N
         IF ( X(I).LE.X(I-1) )  GO TO 5003
    1 CONTINUE
C
C  FUNCTION DEFINITION IS OK, GO ON.
C
      IERR = 0
      NLESS1 = N - 1
      H1 = X(2) - X(1)
      DEL1 = (F(1,2) - F(1,1))/H1
      DSAVE = DEL1
C
C  SPECIAL CASE N=2 -- USE LINEAR INTERPOLATION.
C
      IF (NLESS1 .GT. 1)  GO TO 10
      D(1,1) = DEL1
      D(1,N) = DEL1
      GO TO 5000
C
C  NORMAL CASE  (N .GE. 3).
C
   10 CONTINUE
      H2 = X(3) - X(2)
      DEL2 = (F(1,3) - F(1,2))/H2
C
C  SET D(1) VIA NON-CENTERED THREE-POINT FORMULA, ADJUSTED TO BE
C     SHAPE-PRESERVING.
C
      HSUM = H1 + H2
      W1 = (H1 + HSUM)/HSUM
      W2 = -H1/HSUM
      D(1,1) = W1*DEL1 + W2*DEL2
      IF ( PCHST(D(1,1),DEL1) .LE. ZERO)  THEN
         D(1,1) = ZERO
      ELSE IF ( PCHST(DEL1,DEL2) .LT. ZERO)  THEN
C        NEED DO THIS CHECK ONLY IF MONOTONICITY SWITCHES.
         DMAX = THREE*DEL1
         IF (ABS(D(1,1)) .GT. ABS(DMAX))  D(1,1) = DMAX
      ENDIF
C
C  LOOP THROUGH INTERIOR POINTS.
C
      DO 50  I = 2, NLESS1
         IF (I .EQ. 2)  GO TO 40
C
         H1 = H2
         H2 = X(I+1) - X(I)
         HSUM = H1 + H2
         DEL1 = DEL2
         DEL2 = (F(1,I+1) - F(1,I))/H2
   40    CONTINUE
C
C        SET D(I)=0 UNLESS DATA ARE STRICTLY MONOTONIC.
C
         D(1,I) = ZERO
         IF ( PCHST(DEL1,DEL2) )  42, 41, 45
C
C        COUNT NUMBER OF CHANGES IN DIRECTION OF MONOTONICITY.
C
   41    CONTINUE
         IF (DEL2 .EQ. ZERO)  GO TO 50
         IF ( PCHST(DSAVE,DEL2) .LT. ZERO)  IERR = IERR + 1
         DSAVE = DEL2
         GO TO 50
C
   42    CONTINUE
         IERR = IERR + 1
         DSAVE = DEL2
         GO TO 50
C
C        USE BRODLIE MODIFICATION OF BUTLAND FORMULA.
C
   45    CONTINUE
         HSUMT3 = HSUM+HSUM+HSUM
         W1 = (HSUM + H1)/HSUMT3
         W2 = (HSUM + H2)/HSUMT3
         DMAX = MAX( ABS(DEL1), ABS(DEL2) )
         DMIN = MIN( ABS(DEL1), ABS(DEL2) )
         DRAT1 = DEL1/DMAX
         DRAT2 = DEL2/DMAX
         D(1,I) = DMIN/(W1*DRAT1 + W2*DRAT2)
C
   50 CONTINUE
C
C  SET D(N) VIA NON-CENTERED THREE-POINT FORMULA, ADJUSTED TO BE
C     SHAPE-PRESERVING.
C
      W1 = -H2/HSUM
      W2 = (H2 + HSUM)/HSUM
      D(1,N) = W1*DEL1 + W2*DEL2
      IF ( PCHST(D(1,N),DEL2) .LE. ZERO)  THEN
         D(1,N) = ZERO
      ELSE IF ( PCHST(DEL1,DEL2) .LT. ZERO)  THEN
C        NEED DO THIS CHECK ONLY IF MONOTONICITY SWITCHES.
         DMAX = THREE*DEL2
         IF (ABS(D(1,N)) .GT. ABS(DMAX))  D(1,N) = DMAX
      ENDIF
C
C  NORMAL RETURN.
C
 5000 CONTINUE
      RETURN
C
C  ERROR RETURNS.
C
 5001 CONTINUE
C     N.LT.2 RETURN.
      IERR = -1
      CALL XERMSG ('SLATEC', 'PCHIM',
     +   'NUMBER OF DATA POINTS LESS THAN TWO', IERR, 1)
      RETURN
C
 5002 CONTINUE
C     INCFD.LT.1 RETURN.
      IERR = -2
      CALL XERMSG ('SLATEC', 'PCHIM', 'INCREMENT LESS THAN ONE', IERR,
     +   1)
      RETURN
C
 5003 CONTINUE
C     X-ARRAY NOT STRICTLY INCREASING.
      IERR = -3
      CALL XERMSG ('SLATEC', 'PCHIM', 'X-ARRAY NOT STRICTLY INCREASING'
     +   , IERR, 1)
      RETURN
C------------- LAST LINE OF PCHIM FOLLOWS ------------------------------
      END

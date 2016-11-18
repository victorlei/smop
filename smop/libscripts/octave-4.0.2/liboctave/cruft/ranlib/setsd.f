      SUBROUTINE setsd(iseed1,iseed2)
C**********************************************************************
C
C     SUBROUTINE SETSD(ISEED1,ISEED2)
C               SET S-ee-D of current generator
C
C     Resets the initial  seed of  the current  generator to  ISEED1 and
C     ISEED2. The seeds of the other generators remain unchanged.
C
C     This is a transcription from Pascal to Fortran of routine
C     Set_Seed from the paper
C
C     L'Ecuyer, P. and Cote, S. "Implementing a Random Number Package
C     with Splitting Facilities." ACM Transactions on Mathematical
C     Software, 17:98-111 (1991)
C
C
C                              Arguments
C
C
C     ISEED1 -> First integer seed
C                                   INTEGER ISEED1
C
C     ISEED2 -> Second integer seed
C                                   INTEGER ISEED1
C
C**********************************************************************
C     .. Parameters ..
      INTEGER numg
      PARAMETER (numg=32)
C     ..
C     .. Scalar Arguments ..
      INTEGER iseed1,iseed2
C     ..
C     .. Scalars in Common ..
      INTEGER a1,a1vw,a1w,a2,a2vw,a2w,m1,m2
C     ..
C     .. Arrays in Common ..
      INTEGER cg1(numg),cg2(numg),ig1(numg),ig2(numg),lg1(numg),
     +        lg2(numg)
      LOGICAL qanti(numg)
C     ..
C     .. Local Scalars ..
      INTEGER g
C     ..
C     .. External Functions ..
      LOGICAL qrgnin
      EXTERNAL qrgnin
C     ..
C     .. External Subroutines ..
      EXTERNAL getcgn,initgn
C     ..
C     .. Common blocks ..
      COMMON /globe/m1,m2,a1,a2,a1w,a2w,a1vw,a2vw,ig1,ig2,lg1,lg2,cg1,
     +       cg2,qanti
C     ..
C     .. Save statement ..
      SAVE /globe/
C     ..
C     .. Executable Statements ..
C     Abort unless random number generator initialized
      IF (qrgnin()) GO TO 10
      WRITE (*,*) ' SETSD called before random number generator ',
     +  ' initialized -- abort!'
      CALL XSTOPX
     + (' SETSD called before random number generator initialized')

   10 CALL getcgn(g)
      ig1(g) = iseed1
      ig2(g) = iseed2
      CALL initgn(-1)
      RETURN

      END

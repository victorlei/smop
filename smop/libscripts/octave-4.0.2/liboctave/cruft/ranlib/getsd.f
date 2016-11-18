      SUBROUTINE getsd(iseed1,iseed2)
C**********************************************************************
C
C     SUBROUTINE GETSD(G,ISEED1,ISEED2)
C               GET SeeD
C
C     Returns the value of two integer seeds of the current generator
C
C     This  is   a  transcription from  Pascal   to  Fortran  of routine
C     Get_State from the paper
C
C     L'Ecuyer, P. and  Cote,  S. "Implementing a Random Number  Package
C     with   Splitting Facilities."  ACM  Transactions   on Mathematical
C     Software, 17:98-111 (1991)
C
C
C                              Arguments
C
C
C
C     ISEED1 <- First integer seed of generator G
C                                   INTEGER ISEED1
C
C     ISEED2 <- Second integer seed of generator G
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
      EXTERNAL getcgn
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
      WRITE (*,*) ' GETSD called before random number generator ',
     +  ' initialized -- abort!'
      CALL XSTOPX
     + (' GETSD called before random number generator initialized')

   10 CALL getcgn(g)
      iseed1 = cg1(g)
      iseed2 = cg2(g)
      RETURN

      END

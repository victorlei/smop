      SUBROUTINE setant(qvalue)
C**********************************************************************
C
C      SUBROUTINE SETANT(QVALUE)
C               SET ANTithetic
C
C     Sets whether the current generator produces antithetic values.  If
C     X   is  the value  normally returned  from  a uniform [0,1] random
C     number generator then 1  - X is the antithetic  value. If X is the
C     value  normally  returned  from a   uniform  [0,N]  random  number
C     generator then N - 1 - X is the antithetic value.
C
C     All generators are initialized to NOT generate antithetic values.
C
C     This is a transcription from Pascal to Fortran of routine
C     Set_Antithetic from the paper
C
C     L'Ecuyer, P. and Cote, S. "Implementing a Random Number Package
C     with Splitting Facilities." ACM Transactions on Mathematical
C     Software, 17:98-111 (1991)
C
C
C                              Arguments
C
C
C     QVALUE -> .TRUE. if generator G is to generating antithetic
C                    values, otherwise .FALSE.
C                                   LOGICAL QVALUE
C
C**********************************************************************
C     .. Parameters ..
      INTEGER numg
      PARAMETER (numg=32)
C     ..
C     .. Scalar Arguments ..
      LOGICAL qvalue
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
      WRITE (*,*) ' SETANT called before random number generator ',
     +  ' initialized -- abort!'
      CALL XSTOPX
     + (' SETANT called before random number generator initialized')

   10 CALL getcgn(g)
      qanti(g) = qvalue
      RETURN

      END

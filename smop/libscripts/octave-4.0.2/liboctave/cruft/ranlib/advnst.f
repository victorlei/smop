      SUBROUTINE advnst(k)
C**********************************************************************
C
C     SUBROUTINE ADVNST(K)
C               ADV-a-N-ce ST-ate
C
C     Advances the state  of  the current  generator  by 2^K values  and
C     resets the initial seed to that value.
C
C     This is  a  transcription from   Pascal to  Fortran    of  routine
C     Advance_State from the paper
C
C     L'Ecuyer, P. and  Cote, S. "Implementing  a  Random Number Package
C     with  Splitting   Facilities."  ACM  Transactions  on Mathematical
C     Software, 17:98-111 (1991)
C
C
C                              Arguments
C
C
C     K -> The generator is advanced by2^K values
C                                   INTEGER K
C
C**********************************************************************
C     .. Parameters ..
      INTEGER numg
      PARAMETER (numg=32)
C     ..
C     .. Scalar Arguments ..
      INTEGER k
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
      INTEGER g,i,ib1,ib2
C     ..
C     .. External Functions ..
      INTEGER mltmod
      LOGICAL qrgnin
      EXTERNAL mltmod,qrgnin
C     ..
C     .. External Subroutines ..
      EXTERNAL getcgn,setsd
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
      WRITE (*,*) ' ADVNST called before random number generator ',
     +  ' initialized -- abort!'
      CALL XSTOPX
     + (' ADVNST called before random number generator initialized')

   10 CALL getcgn(g)
C
      ib1 = a1
      ib2 = a2
      DO 20,i = 1,k
          ib1 = mltmod(ib1,ib1,m1)
          ib2 = mltmod(ib2,ib2,m2)
   20 CONTINUE
      CALL setsd(mltmod(ib1,cg1(g),m1),mltmod(ib2,cg2(g),m2))
C
C     NOW, IB1 = A1**K AND IB2 = A2**K
C
      RETURN

      END

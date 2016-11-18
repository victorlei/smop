      SUBROUTINE inrgcm()
C**********************************************************************
C
C     SUBROUTINE INRGCM()
C          INitialize Random number Generator CoMmon
C
C
C                              Function
C
C
C     Initializes common area  for random number  generator.  This saves
C     the  nuisance  of  a  BLOCK DATA  routine  and the  difficulty  of
C     assuring that the routine is loaded with the other routines.
C
C**********************************************************************
C     .. Parameters ..
      INTEGER numg
      PARAMETER (numg=32)
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
      INTEGER i
      LOGICAL qdum
C     ..
C     .. External Functions ..
      LOGICAL qrgnsn
      EXTERNAL qrgnsn
C     ..
C     .. Common blocks ..
      COMMON /globe/m1,m2,a1,a2,a1w,a2w,a1vw,a2vw,ig1,ig2,lg1,lg2,cg1,
     +       cg2,qanti
C     ..
C     .. Save statement ..
      SAVE /globe/
C     ..
C     .. Executable Statements ..
C     V=20;                            W=30;
C
C     A1W = MOD(A1**(2**W),M1)         A2W = MOD(A2**(2**W),M2)
C     A1VW = MOD(A1**(2**(V+W)),M1)    A2VW = MOD(A2**(2**(V+W)),M2)
C
C   If V or W is changed A1W, A2W, A1VW, and A2VW need to be recomputed.
C    An efficient way to precompute a**(2*j) MOD m is to start with
C    a and square it j times modulo m using the function MLTMOD.
C
      m1 = 2147483563
      m2 = 2147483399
      a1 = 40014
      a2 = 40692
      a1w = 1033780774
      a2w = 1494757890
      a1vw = 2082007225
      a2vw = 784306273
      DO 10,i = 1,numg
          qanti(i) = .FALSE.
   10 CONTINUE
C
C     Tell the world that common has been initialized
C
      qdum = qrgnsn(.TRUE.)
      RETURN

      END

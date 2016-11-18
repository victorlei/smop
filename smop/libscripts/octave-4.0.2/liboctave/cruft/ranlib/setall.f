      SUBROUTINE setall(iseed1,iseed2)
C**********************************************************************
C
C      SUBROUTINE SETALL(ISEED1,ISEED2)
C               SET ALL random number generators
C
C     Sets the initial seed of generator 1 to ISEED1 and ISEED2. The
C     initial seeds of the other generators are set accordingly, and
C     all generators states are set to these seeds.
C
C     This is a transcription from Pascal to Fortran of routine
C     Set_Initial_Seed from the paper
C
C     L'Ecuyer, P. and Cote, S. "Implementing a Random Number Package
C     with Splitting Facilities." ACM Transactions on Mathematical
C     Software, 17:98-111 (1991)
C
C
C                              Arguments
C
C
C     ISEED1 -> First of two integer seeds
C                                   INTEGER ISEED1
C
C     ISEED2 -> Second of two integer seeds
C                                   INTEGER ISEED1
C
C**********************************************************************
C     .. Parameters ..
      INTEGER numg
      PARAMETER (numg=32)
C     ..
C     .. Scalar Arguments ..
      INTEGER iseed1,iseed2
      LOGICAL qssd
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
      INTEGER g,ocgn
      LOGICAL qqssd
C     ..
C     .. External Functions ..
      INTEGER mltmod
      LOGICAL qrgnin
      EXTERNAL mltmod,qrgnin
C     ..
C     .. External Subroutines ..
      EXTERNAL getcgn,initgn,inrgcm,setcgn
C     ..
C     .. Common blocks ..
      COMMON /globe/m1,m2,a1,a2,a1w,a2w,a1vw,a2vw,ig1,ig2,lg1,lg2,cg1,
     +       cg2,qanti
C     ..
C     .. Save statement ..
      SAVE /globe/,qqssd
C     ..
C     .. Data statements ..
      DATA qqssd/.FALSE./
C     ..
C     .. Executable Statements ..
C
C     TELL IGNLGI, THE ACTUAL NUMBER GENERATOR, THAT THIS ROUTINE
C      HAS BEEN CALLED.
C
      qqssd = .TRUE.
      CALL getcgn(ocgn)
C
C     Initialize Common Block if Necessary
C
      IF (.NOT. (qrgnin())) CALL inrgcm()
      ig1(1) = iseed1
      ig2(1) = iseed2
      CALL initgn(-1)
      DO 10,g = 2,numg
          ig1(g) = mltmod(a1vw,ig1(g-1),m1)
          ig2(g) = mltmod(a2vw,ig2(g-1),m2)
          CALL setcgn(g)
          CALL initgn(-1)
   10 CONTINUE
      CALL setcgn(ocgn)
      RETURN

      ENTRY rgnqsd(qssd)
C**********************************************************************
C
C     SUBROUTINE RGNQSD
C                    Random Number Generator Query SeeD set?
C
C     Returns (LOGICAL) QSSD as .TRUE. if SETALL has been invoked,
C     otherwise returns .FALSE.
C
C**********************************************************************
      qssd = qqssd
      RETURN

      END

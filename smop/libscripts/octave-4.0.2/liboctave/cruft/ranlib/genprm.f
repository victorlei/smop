      SUBROUTINE genprm(iarray,larray)
C**********************************************************************
C
C    SUBROUTINE GENPRM( IARRAY, LARRAY )
C               GENerate random PeRMutation of iarray
C
C
C                              Arguments
C
C
C     IARRAY <--> On output IARRAY is a random permutation of its
C                 value on input
C                         INTEGER IARRAY( LARRAY )
C
C     LARRAY <--> Length of IARRAY
C                         INTEGER LARRAY
C
C**********************************************************************
C     .. Scalar Arguments ..
      INTEGER larray
C     ..
C     .. Array Arguments ..
      INTEGER iarray(larray)
C     ..
C     .. Local Scalars ..
      INTEGER i,itmp,iwhich
C     ..
C     .. External Functions ..
      INTEGER ignuin
      EXTERNAL ignuin
C     ..
C     .. Executable Statements ..
      DO 10,i = 1,larray
          iwhich = ignuin(i,larray)
          itmp = iarray(iwhich)
          iarray(iwhich) = iarray(i)
          iarray(i) = itmp
   10 CONTINUE
      RETURN

      END

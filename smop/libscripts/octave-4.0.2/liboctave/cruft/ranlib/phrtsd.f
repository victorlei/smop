      SUBROUTINE phrtsd(phrase,seed1,seed2)
C**********************************************************************
C
C     SUBROUTINE PHRTSD( PHRASE, SEED1, SEED2 )
C               PHRase To SeeDs
C
C
C                              Function
C
C
C     Uses a phrase (character string) to generate two seeds for the RGN
C     random number generator.
C
C
C                              Arguments
C
C
C     PHRASE --> Phrase to be used for random number generation
C                         CHARACTER*(*) PHRASE
C
C     SEED1 <-- First seed for RGN generator
C                         INTEGER SEED1
C
C     SEED2 <-- Second seed for RGN generator
C                         INTEGER SEED2
C
C
C                              Note
C
C
C     Trailing blanks are eliminated before the seeds are generated.
C
C     Generated seed values will fall in the range 1..2^30
C     (1..1,073,741,824)
C
C**********************************************************************
C     .. Parameters ..
      CHARACTER*(*) table
      PARAMETER (table='abcdefghijklmnopqrstuvwxyz'//
     +          'ABCDEFGHIJKLMNOPQRSTUVWXYZ'//'0123456789'//
     +          '!@#$%^&*()_+[];:''"<>?,./')
      INTEGER twop30
      PARAMETER (twop30=1073741824)
      INTEGER sixty4
      PARAMETER (sixty4=64)
C     ..
C     .. Scalar Arguments ..
      INTEGER seed1,seed2
      CHARACTER phrase* (*)
C     ..
C     .. Local Scalars ..
      INTEGER i,ichr,j,lphr,idxval
C     ..
C     .. Local Arrays ..
      INTEGER shift(0:4),values(5)
C     ..
C     .. External Functions ..
      INTEGER lennob
      EXTERNAL lennob
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC index,mod
C     ..
C     JJV added Save statement for variable in Data statement
C     .. Save statements ..
      SAVE shift
C     JJV end addition
C     ..
C     .. Data statements ..
      DATA shift/1,64,4096,262144,16777216/
C     ..
C     .. Executable Statements ..
      seed1 = 1234567890
      seed2 = 123456789
      lphr = lennob(phrase)
      IF (lphr.LT.1) RETURN
      DO 30,i = 1,lphr
          idxval = index(table,phrase(i:i))
          ichr = mod(idxval,sixty4)
          IF (ichr.EQ.0) ichr = 63
          DO 10,j = 1,5
              values(j) = ichr - j
              IF (values(j).LT.1) values(j) = values(j) + 63
   10     CONTINUE
          DO 20,j = 1,5
              seed1 = mod(seed1+shift(j-1)*values(j),twop30)
              seed2 = mod(seed2+shift(j-1)*values(6-j),twop30)
   20     CONTINUE
   30 CONTINUE
      RETURN

      END

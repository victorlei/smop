      REAL FUNCTION gennor(av,sd)
C**********************************************************************
C
C     REAL FUNCTION GENNOR( AV, SD )
C
C         GENerate random deviate from a NORmal distribution
C
C
C                              Function
C
C
C     Generates a single random deviate from a normal distribution
C     with mean, AV, and standard deviation, SD.
C
C
C                              Arguments
C
C
C     AV --> Mean of the normal distribution.
C                              REAL AV
C
C     SD --> Standard deviation of the normal distribution.
C                              REAL SD
C     JJV                      (SD >= 0)
C
C     GENNOR <-- Generated normal deviate.
C                              REAL GENNOR
C
C
C                              Method
C
C
C     Renames SNORM from TOMS as slightly modified by BWB to use RANF
C     instead of SUNIF.
C
C     For details see:
C               Ahrens, J.H. and Dieter, U.
C               Extensions of Forsythe's Method for Random
C               Sampling from the Normal Distribution.
C               Math. Comput., 27,124 (Oct. 1973), 927 - 937.
C
C
C**********************************************************************
C     .. Scalar Arguments ..
      REAL av,sd
C     ..
C     .. External Functions ..
      REAL snorm
      EXTERNAL snorm
C     ..
C     .. Executable Statements ..
C     JJV added check to ensure SD >= 0.0
      IF (sd.GE.0.0) GO TO 10
      WRITE (*,*) 'SD < 0.0 in GENNOR - ABORT'
      WRITE (*,*) 'Value of SD: ',sd
      CALL XSTOPX ('SD < 0.0 in GENNOR - ABORT')

 10   gennor = sd*snorm() + av
      RETURN

      END

      REAL FUNCTION genexp(av)

C**********************************************************************
C
C     REAL FUNCTION GENEXP( AV )
C
C                    GENerate EXPonential random deviate
C
C
C                              Function
C
C
C     Generates a single random deviate from an exponential
C     distribution with mean AV.
C
C
C                              Arguments
C
C
C     AV --> The mean of the exponential distribution from which
C            a random deviate is to be generated.
C                              REAL AV
C     JJV                      (AV >= 0)
C
C     GENEXP <-- The random deviate.
C                              REAL GENEXP
C
C
C                              Method
C
C
C     Renames SEXPO from TOMS as slightly modified by BWB to use RANF
C     instead of SUNIF.
C
C     For details see:
C
C               Ahrens, J.H. and Dieter, U.
C               Computer Methods for Sampling From the
C               Exponential and Normal Distributions.
C               Comm. ACM, 15,10 (Oct. 1972), 873 - 882.
C
C**********************************************************************
C     .. Scalar Arguments ..
      REAL av
C     ..
C     .. External Functions ..
      REAL sexpo
      EXTERNAL sexpo
C     ..
C     .. Executable Statements ..
C     JJV added check to ensure AV >= 0.0
      IF (av.GE.0.0) GO TO 10
      WRITE (*,*) 'AV < 0.0 in GENEXP - ABORT'
      WRITE (*,*) 'Value of AV: ',av
      CALL XSTOPX ('AV < 0.0 in GENEXP - ABORT')

 10   genexp = sexpo()*av
      RETURN

      END

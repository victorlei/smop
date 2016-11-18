      REAL FUNCTION genunf(low,high)
C**********************************************************************
C
C     REAL FUNCTION GENUNF( LOW, HIGH )
C
C               GeNerate Uniform Real between LOW and HIGH
C
C
C                              Function
C
C
C     Generates a real uniformly distributed between LOW and HIGH.
C
C
C                              Arguments
C
C
C     LOW --> Low bound (exclusive) on real value to be generated
C                         REAL LOW
C
C     HIGH --> High bound (exclusive) on real value to be generated
C                         REAL HIGH
C
C**********************************************************************
C     .. Scalar Arguments ..
      REAL high,low
C     ..
C     .. External Functions ..
      REAL ranf
      EXTERNAL ranf
C     ..
C     .. Executable Statements ..
      IF (.NOT. (low.GT.high)) GO TO 10
      WRITE (*,*) 'LOW > HIGH in GENUNF: LOW ',low,' HIGH: ',high
      WRITE (*,*) 'Abort'
      CALL XSTOPX ('LOW > High in GENUNF - Abort')

   10 genunf = low + (high-low)*ranf()

      RETURN

      END

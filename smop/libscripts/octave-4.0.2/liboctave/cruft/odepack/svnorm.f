      REAL FUNCTION SVNORM (N, V, W)
C***BEGIN PROLOGUE  SVNORM
C***SUBSIDIARY
C***PURPOSE  Weighted root-mean-square vector norm.
C***TYPE      SINGLE PRECISION (SVNORM-S, DVNORM-D)
C***AUTHOR  Hindmarsh, Alan C., (LLNL)
C***DESCRIPTION
C
C  This function routine computes the weighted root-mean-square norm
C  of the vector of length N contained in the array V, with weights
C  contained in the array W of length N:
C    SVNORM = SQRT( (1/N) * SUM( V(i)*W(i) )**2 )
C
C***SEE ALSO  SLSODE
C***ROUTINES CALLED  (NONE)
C***REVISION HISTORY  (YYMMDD)
C   791129  DATE WRITTEN
C   890501  Modified prologue to SLATEC/LDOC format.  (FNF)
C   890503  Minor cosmetic changes.  (FNF)
C   930809  Renamed to allow single/double precision versions. (ACH)
C***END PROLOGUE  SVNORM
C**End
      INTEGER N,   I
      REAL V, W,   SUM
      DIMENSION V(N), W(N)
C
C***FIRST EXECUTABLE STATEMENT  SVNORM
      SUM = 0.0E0
      DO 10 I = 1,N
 10     SUM = SUM + (V(I)*W(I))**2
      SVNORM = SQRT(SUM/N)
      RETURN
C----------------------- END OF FUNCTION SVNORM ------------------------
      END

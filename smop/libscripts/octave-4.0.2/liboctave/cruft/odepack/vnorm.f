      DOUBLE PRECISION FUNCTION VNORM (N, V, W)
CLLL. OPTIMIZE
C-----------------------------------------------------------------------
C THIS FUNCTION ROUTINE COMPUTES THE WEIGHTED ROOT-MEAN-SQUARE NORM
C OF THE VECTOR OF LENGTH N CONTAINED IN THE ARRAY V, WITH WEIGHTS
C CONTAINED IN THE ARRAY W OF LENGTH N..
C   VNORM = SQRT( (1/N) * SUM( V(I)*W(I) )**2 )
C-----------------------------------------------------------------------
      INTEGER N,   I
      DOUBLE PRECISION V, W,   SUM
      DIMENSION V(N), W(N)
      SUM = 0.0D0
      DO 10 I = 1,N
 10     SUM = SUM + (V(I)*W(I))**2
      VNORM = DSQRT(SUM/DBLE(N))
      RETURN
C----------------------- END OF FUNCTION VNORM -------------------------
      END

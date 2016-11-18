      SUBROUTINE EWSET (N, ITOL, RTOL, ATOL, YCUR, EWT)
CLLL. OPTIMIZE
C-----------------------------------------------------------------------
C THIS SUBROUTINE SETS THE ERROR WEIGHT VECTOR EWT ACCORDING TO
C     EWT(I) = RTOL(I)*ABS(YCUR(I)) + ATOL(I),  I = 1,...,N,
C WITH THE SUBSCRIPT ON RTOL AND/OR ATOL POSSIBLY REPLACED BY 1 ABOVE,
C DEPENDING ON THE VALUE OF ITOL.
C-----------------------------------------------------------------------
      INTEGER N, ITOL
      INTEGER I
      DOUBLE PRECISION RTOL, ATOL, YCUR, EWT
      DIMENSION RTOL(*), ATOL(*), YCUR(N), EWT(N)
C
      GO TO (10, 20, 30, 40), ITOL
 10   CONTINUE
      DO 15 I = 1,N
 15     EWT(I) = RTOL(1)*DABS(YCUR(I)) + ATOL(1)
      RETURN
 20   CONTINUE
      DO 25 I = 1,N
 25     EWT(I) = RTOL(1)*DABS(YCUR(I)) + ATOL(I)
      RETURN
 30   CONTINUE
      DO 35 I = 1,N
 35     EWT(I) = RTOL(I)*DABS(YCUR(I)) + ATOL(1)
      RETURN
 40   CONTINUE
      DO 45 I = 1,N
 45     EWT(I) = RTOL(I)*DABS(YCUR(I)) + ATOL(I)
      RETURN
C----------------------- END OF SUBROUTINE EWSET -----------------------
      END

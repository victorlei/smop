C Work performed under the auspices of the U.S. Department of Energy
C by Lawrence Livermore National Laboratory under contract number
C W-7405-Eng-48.
C
      SUBROUTINE DHEQR (A, LDA, N, Q, INFO, IJOB)
C
C***BEGIN PROLOGUE  DHEQR
C***DATE WRITTEN   890101   (YYMMDD)
C***REVISION DATE  900926   (YYMMDD)
C
C-----------------------------------------------------------------------
C***DESCRIPTION
C
C     This routine performs a QR decomposition of an upper
C     Hessenberg matrix A.  There are two options available:
C
C          (1)  performing a fresh decomposition
C          (2)  updating the QR factors by adding a row and A
C               column to the matrix A.
C
C     DHEQR decomposes an upper Hessenberg matrix by using Givens
C     rotations.
C
C     On entry
C
C        A       DOUBLE PRECISION(LDA, N)
C                The matrix to be decomposed.
C
C        LDA     INTEGER
C                The leading dimension of the array A.
C
C        N       INTEGER
C                A is an (N+1) by N Hessenberg matrix.
C
C        IJOB    INTEGER
C                = 1     Means that a fresh decomposition of the
C                        matrix A is desired.
C                .GE. 2  Means that the current decomposition of A
C                        will be updated by the addition of a row
C                        and a column.
C     On return
C
C        A       The upper triangular matrix R.
C                The factorization can be written Q*A = R, where
C                Q is a product of Givens rotations and R is upper
C                triangular.
C
C        Q       DOUBLE PRECISION(2*N)
C                The factors C and S of each Givens rotation used
C                in decomposing A.
C
C        INFO    INTEGER
C                = 0  normal value.
C                = K  If  A(K,K) .EQ. 0.0.  This is not an error
C                     condition for this subroutine, but it does
C                     indicate that DHELS will divide by zero
C                     if called.
C
C     Modification of LINPACK.
C     Peter Brown, Lawrence Livermore Natl. Lab.
C
C-----------------------------------------------------------------------
C***ROUTINES CALLED (NONE)
C
C***END PROLOGUE  DHEQR
C
      INTEGER LDA, N, INFO, IJOB
      DOUBLE PRECISION A(LDA,*), Q(*)
      INTEGER I, IQ, J, K, KM1, KP1, NM1
      DOUBLE PRECISION C, S, T, T1, T2
C
      IF (IJOB .GT. 1) GO TO 70
C-----------------------------------------------------------------------
C A new factorization is desired.
C-----------------------------------------------------------------------
C
C     QR decomposition without pivoting.
C
      INFO = 0
      DO 60 K = 1, N
         KM1 = K - 1
         KP1 = K + 1
C
C           Compute Kth column of R.
C           First, multiply the Kth column of A by the previous
C           K-1 Givens rotations.
C
            IF (KM1 .LT. 1) GO TO 20
            DO 10 J = 1, KM1
              I = 2*(J-1) + 1
              T1 = A(J,K)
              T2 = A(J+1,K)
              C = Q(I)
              S = Q(I+1)
              A(J,K) = C*T1 - S*T2
              A(J+1,K) = S*T1 + C*T2
   10         CONTINUE
C
C           Compute Givens components C and S.
C
   20       CONTINUE
            IQ = 2*KM1 + 1
            T1 = A(K,K)
            T2 = A(KP1,K)
            IF (T2 .NE. 0.0D0) GO TO 30
              C = 1.0D0
              S = 0.0D0
              GO TO 50
   30       CONTINUE
            IF (ABS(T2) .LT. ABS(T1)) GO TO 40
              T = T1/T2
              S = -1.0D0/SQRT(1.0D0+T*T)
              C = -S*T
              GO TO 50
   40       CONTINUE
              T = T2/T1
              C = 1.0D0/SQRT(1.0D0+T*T)
              S = -C*T
   50       CONTINUE
            Q(IQ) = C
            Q(IQ+1) = S
            A(K,K) = C*T1 - S*T2
            IF (A(K,K) .EQ. 0.0D0) INFO = K
   60 CONTINUE
      RETURN
C-----------------------------------------------------------------------
C The old factorization of A will be updated.  A row and a column
C has been added to the matrix A.
C N by N-1 is now the old size of the matrix.
C-----------------------------------------------------------------------
  70  CONTINUE
      NM1 = N - 1
C-----------------------------------------------------------------------
C Multiply the new column by the N previous Givens rotations.
C-----------------------------------------------------------------------
      DO 100 K = 1,NM1
        I = 2*(K-1) + 1
        T1 = A(K,N)
        T2 = A(K+1,N)
        C = Q(I)
        S = Q(I+1)
        A(K,N) = C*T1 - S*T2
        A(K+1,N) = S*T1 + C*T2
 100    CONTINUE
C-----------------------------------------------------------------------
C Complete update of decomposition by forming last Givens rotation,
C and multiplying it times the column vector (A(N,N),A(NP1,N)).
C-----------------------------------------------------------------------
      INFO = 0
      T1 = A(N,N)
      T2 = A(N+1,N)
      IF (T2 .NE. 0.0D0) GO TO 110
        C = 1.0D0
        S = 0.0D0
        GO TO 130
 110  CONTINUE
      IF (ABS(T2) .LT. ABS(T1)) GO TO 120
        T = T1/T2
        S = -1.0D0/SQRT(1.0D0+T*T)
        C = -S*T
        GO TO 130
 120  CONTINUE
        T = T2/T1
        C = 1.0D0/SQRT(1.0D0+T*T)
        S = -C*T
 130  CONTINUE
      IQ = 2*N - 1
      Q(IQ) = C
      Q(IQ+1) = S
      A(N,N) = C*T1 - S*T2
      IF (A(N,N) .EQ. 0.0D0) INFO = N
      RETURN
C
C------END OF SUBROUTINE DHEQR------------------------------------------
      END

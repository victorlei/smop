C Work performed under the auspices of the U.S. Department of Energy
C by Lawrence Livermore National Laboratory under contract number
C W-7405-Eng-48.
C
      SUBROUTINE DHELS (A, LDA, N, Q, B)
C
C***BEGIN PROLOGUE  DHELS
C***DATE WRITTEN   890101   (YYMMDD)
C***REVISION DATE  900926   (YYMMDD)
C
C
C-----------------------------------------------------------------------
C***DESCRIPTION
C
C This is similar to the LINPACK routine DGESL except that
C A is an upper Hessenberg matrix.
C
C     DHELS solves the least squares problem
C
C           MIN (B-A*X,B-A*X)
C
C     using the factors computed by DHEQR.
C
C     On entry
C
C        A       DOUBLE PRECISION (LDA, N)
C                The output from DHEQR which contains the upper
C                triangular factor R in the QR decomposition of A.
C
C        LDA     INTEGER
C                The leading dimension of the array  A .
C
C        N       INTEGER
C                A is originally an (N+1) by N matrix.
C
C        Q       DOUBLE PRECISION(2*N)
C                The coefficients of the N givens rotations
C                used in the QR factorization of A.
C
C        B       DOUBLE PRECISION(N+1)
C                The right hand side vector.
C
C
C     On return
C
C        B       The solution vector X.
C
C
C     Modification of LINPACK.
C     Peter Brown, Lawrence Livermore Natl. Lab.
C
C-----------------------------------------------------------------------
C***ROUTINES CALLED
C   DAXPY
C
C***END PROLOGUE  DHELS
C
      INTEGER LDA, N
      DOUBLE PRECISION A(LDA,*), B(*), Q(*)
      INTEGER IQ, K, KB, KP1
      DOUBLE PRECISION C, S, T, T1, T2
C
C        Minimize (B-A*X,B-A*X).
C        First form Q*B.
C
         DO 20 K = 1, N
            KP1 = K + 1
            IQ = 2*(K-1) + 1
            C = Q(IQ)
            S = Q(IQ+1)
            T1 = B(K)
            T2 = B(KP1)
            B(K) = C*T1 - S*T2
            B(KP1) = S*T1 + C*T2
   20    CONTINUE
C
C        Now solve R*X = Q*B.
C
         DO 40 KB = 1, N
            K = N + 1 - KB
            B(K) = B(K)/A(K,K)
            T = -B(K)
            CALL DAXPY (K-1, T, A(1,K), 1, B(1), 1)
   40    CONTINUE
      RETURN
C
C------END OF SUBROUTINE DHELS------------------------------------------
      END

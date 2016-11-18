C Work performed under the auspices of the U.S. Department of Energy
C by Lawrence Livermore National Laboratory under contract number
C W-7405-Eng-48.
C
      SUBROUTINE DORTH (VNEW, V, HES, N, LL, LDHES, KMP, SNORMW)
C
C***BEGIN PROLOGUE  DORTH
C***DATE WRITTEN   890101   (YYMMDD)
C***REVISION DATE  900926   (YYMMDD)
C
C
C-----------------------------------------------------------------------
C***DESCRIPTION
C
C This routine orthogonalizes the vector VNEW against the previous
C KMP vectors in the V array.  It uses a modified Gram-Schmidt
C orthogonalization procedure with conditional reorthogonalization.
C
C      On entry
C
C         VNEW = The vector of length N containing a scaled product
C                OF The Jacobian and the vector V(*,LL).
C
C         V    = The N x LL array containing the previous LL
C                orthogonal vectors V(*,1) to V(*,LL).
C
C         HES  = An LL x LL upper Hessenberg matrix containing,
C                in HES(I,K), K.LT.LL, scaled inner products of
C                A*V(*,K) and V(*,I).
C
C        LDHES = The leading dimension of the HES array.
C
C         N    = The order of the matrix A, and the length of VNEW.
C
C         LL   = The current order of the matrix HES.
C
C          KMP = The number of previous vectors the new vector VNEW
C                must be made orthogonal to (KMP .LE. MAXL).
C
C
C      On return
C
C         VNEW = The new vector orthogonal to V(*,I0),
C                where I0 = MAX(1, LL-KMP+1).
C
C         HES  = Upper Hessenberg matrix with column LL filled in with
C                scaled inner products of A*V(*,LL) and V(*,I).
C
C       SNORMW = L-2 norm of VNEW.
C
C-----------------------------------------------------------------------
C***ROUTINES CALLED
C   DDOT, DNRM2, DAXPY
C
C***END PROLOGUE  DORTH
C
      INTEGER N, LL, LDHES, KMP
      DOUBLE PRECISION VNEW, V, HES, SNORMW
      DIMENSION VNEW(*), V(N,*), HES(LDHES,*)
      INTEGER I, I0
      DOUBLE PRECISION ARG, DDOT, DNRM2, SUMDSQ, TEM, VNRM
C
C-----------------------------------------------------------------------
C Get norm of unaltered VNEW for later use.
C-----------------------------------------------------------------------
      VNRM = DNRM2 (N, VNEW, 1)
C-----------------------------------------------------------------------
C Do Modified Gram-Schmidt on VNEW = A*V(LL).
C Scaled inner products give new column of HES.
C Projections of earlier vectors are subtracted from VNEW.
C-----------------------------------------------------------------------
      I0 = MAX0(1,LL-KMP+1)
      DO 10 I = I0,LL
        HES(I,LL) = DDOT (N, V(1,I), 1, VNEW, 1)
        TEM = -HES(I,LL)
        CALL DAXPY (N, TEM, V(1,I), 1, VNEW, 1)
 10     CONTINUE
C-----------------------------------------------------------------------
C Compute SNORMW = norm of VNEW.
C If VNEW is small compared to its input value (in norm), then
C Reorthogonalize VNEW to V(*,1) through V(*,LL).
C Correct if relative correction exceeds 1000*(unit roundoff).
C Finally, correct SNORMW using the dot products involved.
C-----------------------------------------------------------------------
      SNORMW = DNRM2 (N, VNEW, 1)
      IF (VNRM + 0.001D0*SNORMW .NE. VNRM) RETURN
      SUMDSQ = 0.0D0
      DO 30 I = I0,LL
        TEM = -DDOT (N, V(1,I), 1, VNEW, 1)
        IF (HES(I,LL) + 0.001D0*TEM .EQ. HES(I,LL)) GO TO 30
        HES(I,LL) = HES(I,LL) - TEM
        CALL DAXPY (N, TEM, V(1,I), 1, VNEW, 1)
        SUMDSQ = SUMDSQ + TEM**2
 30     CONTINUE
      IF (SUMDSQ .EQ. 0.0D0) RETURN
      ARG = MAX(0.0D0,SNORMW**2 - SUMDSQ)
      SNORMW = SQRT(ARG)
      RETURN
C
C------END OF SUBROUTINE DORTH------------------------------------------
      END

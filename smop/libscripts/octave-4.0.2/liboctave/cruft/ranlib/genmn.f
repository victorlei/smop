      SUBROUTINE genmn(parm,x,work)
C**********************************************************************
C
C     SUBROUTINE GENMN(PARM,X,WORK)
C              GENerate Multivariate Normal random deviate
C
C
C                              Arguments
C
C
C     PARM --> Parameters needed to generate multivariate normal
C               deviates (MEANV and Cholesky decomposition of
C               COVM). Set by a previous call to SETGMN.
C               1 : 1                - size of deviate, P
C               2 : P + 1            - mean vector
C               P+2 : P*(P+3)/2 + 1  - upper half of cholesky
C                                       decomposition of cov matrix
C                                             REAL PARM(*)
C
C     X    <-- Vector deviate generated.
C                                             REAL X(P)
C
C     WORK <--> Scratch array
C                                             REAL WORK(P)
C
C
C                              Method
C
C
C     1) Generate P independent standard normal deviates - Ei ~ N(0,1)
C
C     2) Using Cholesky decomposition find A s.t. trans(A)*A = COVM
C
C     3) trans(A)E + MEANV ~ N(MEANV,COVM)
C
C**********************************************************************
C     .. Array Arguments ..
      REAL parm(*),work(*),x(*)
C     ..
C     .. Local Scalars ..
      REAL ae
      INTEGER i,icount,j,p
C     ..
C     .. External Functions ..
      REAL snorm
      EXTERNAL snorm
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC int
C     ..
C     .. Executable Statements ..
      p = int(parm(1))
C
C     Generate P independent normal deviates - WORK ~ N(0,1)
C
      DO 10,i = 1,p
          work(i) = snorm()
   10 CONTINUE
      DO 30,i = 1,p
C
C     PARM (P+2 : P*(P+3)/2 + 1) contains A, the Cholesky
C      decomposition of the desired covariance matrix.
C          trans(A)(1,1) = PARM(P+2)
C          trans(A)(2,1) = PARM(P+3)
C          trans(A)(2,2) = PARM(P+2+P)
C          trans(A)(3,1) = PARM(P+4)
C          trans(A)(3,2) = PARM(P+3+P)
C          trans(A)(3,3) = PARM(P+2-1+2P)  ...
C
C     trans(A)*WORK + MEANV ~ N(MEANV,COVM)
C
          icount = 0
          ae = 0.0
          DO 20,j = 1,i
              icount = icount + j - 1
              ae = ae + parm(i+ (j-1)*p-icount+p+1)*work(j)
   20     CONTINUE
          x(i) = ae + parm(i+1)
   30 CONTINUE
      RETURN
C
      END

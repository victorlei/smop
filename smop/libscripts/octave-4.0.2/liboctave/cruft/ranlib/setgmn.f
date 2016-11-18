      SUBROUTINE setgmn(meanv,covm,ldcovm,p,parm)
C      SUBROUTINE setgmn(meanv,covm,p,parm)
C     JJV changed this routine to take leading dimension of COVM
C     JJV argument and pass it to SPOTRF, making it easier to use
C     JJV if the COVM which is used is contained in a larger matrix
C     JJV and to make the routine more consistent with LAPACK.
C     JJV Changes are in comments, declarations, and the call to SPOTRF.
C**********************************************************************
C
C     SUBROUTINE SETGMN( MEANV, COVM, LDCOVM, P, PARM)
C            SET Generate Multivariate Normal random deviate
C
C
C                              Function
C
C
C      Places P, MEANV, and the Cholesky factoriztion of COVM
C      in PARM for GENMN.
C
C
C                              Arguments
C
C
C     MEANV --> Mean vector of multivariate normal distribution.
C                                        REAL MEANV(P)
C
C     COVM   <--> (Input) Covariance   matrix    of  the  multivariate
C                 normal distribution.  This routine uses only the
C                 (1:P,1:P) slice of COVM, but needs to know LDCOVM.
C
C                 (Output) Destroyed on output
C                                        REAL COVM(LDCOVM,P)
C
C     LDCOVM --> Leading actual dimension of COVM.
C                                        INTEGER LDCOVM
C
C     P     --> Dimension of the normal, or length of MEANV.
C                                        INTEGER P
C
C     PARM <-- Array of parameters needed to generate multivariate
C                normal deviates (P, MEANV and Cholesky decomposition
C                of COVM).
C                1 : 1                - P
C                2 : P + 1            - MEANV
C                P+2 : P*(P+3)/2 + 1  - Cholesky decomposition of COVM
C                                             REAL PARM(P*(P+3)/2 + 1)
C
C**********************************************************************
C     .. Scalar Arguments ..
C      INTEGER p
      INTEGER p, ldcovm
C     ..
C     .. Array Arguments ..
C      REAL covm(p,p),meanv(p),parm(p* (p+3)/2+1)
      REAL covm(ldcovm,p),meanv(p),parm(p* (p+3)/2+1)
C     ..
C     .. Local Scalars ..
      INTEGER i,icount,info,j
C     ..
C     .. External Subroutines ..
      EXTERNAL spotrf
C     ..
C     .. Executable Statements ..
C
C
C     TEST THE INPUT
C
      IF (.NOT. (p.LE.0)) GO TO 10
      WRITE (*,*) 'P nonpositive in SETGMN'
      WRITE (*,*) 'Value of P: ',p
      CALL XSTOPX ('P nonpositive in SETGMN')

   10 parm(1) = p
C
C     PUT P AND MEANV INTO PARM
C
      DO 20,i = 2,p + 1
          parm(i) = meanv(i-1)
   20 CONTINUE
C
C      Cholesky decomposition to find A s.t. trans(A)*(A) = COVM
C
C      CALL spofa(covm,p,p,info)
C      CALL spofa(covm,ldcovm,p,info)
      CALL spotrf ( 'Upper', p, covm, ldcovm, info)
      IF (.NOT. (info.NE.0)) GO TO 30
      WRITE (*,*) ' COVM not positive definite in SETGMN'
      CALL XSTOPX (' COVM not positive definite in SETGMN')

   30 icount = p + 1
C
C     PUT UPPER HALF OF A, WHICH IS NOW THE CHOLESKY FACTOR, INTO PARM
C          COVM(1,1) = PARM(P+2)
C          COVM(1,2) = PARM(P+3)
C                    :
C          COVM(1,P) = PARM(2P+1)
C          COVM(2,2) = PARM(2P+2)  ...
C
      DO 50,i = 1,p
          DO 40,j = i,p
              icount = icount + 1
              parm(icount) = covm(i,j)
   40     CONTINUE
   50 CONTINUE
      RETURN
C
      END

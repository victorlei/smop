C Work performed under the auspices of the U.S. Department of Energy
C by Lawrence Livermore National Laboratory under contract number
C W-7405-Eng-48.
C
      SUBROUTINE DMATD(NEQ,X,Y,YPRIME,DELTA,CJ,H,IER,EWT,E,
     *                 WM,IWM,RES,IRES,UROUND,JACD,RPAR,IPAR)
C
C***BEGIN PROLOGUE  DMATD
C***REFER TO  DDASPK
C***DATE WRITTEN   890101   (YYMMDD)
C***REVISION DATE  900926   (YYMMDD)
C***REVISION DATE  940701   (YYMMDD) (new LIPVT)
C
C-----------------------------------------------------------------------
C***DESCRIPTION
C
C     This routine computes the iteration matrix
C     J = dG/dY+CJ*dG/dYPRIME (where G(X,Y,YPRIME)=0).
C     Here J is computed by:
C       the user-supplied routine JACD if IWM(MTYPE) is 1 or 4, or
C       by numerical difference quotients if IWM(MTYPE) is 2 or 5.
C
C     The parameters have the following meanings.
C     X        = Independent variable.
C     Y        = Array containing predicted values.
C     YPRIME   = Array containing predicted derivatives.
C     DELTA    = Residual evaluated at (X,Y,YPRIME).
C                (Used only if IWM(MTYPE)=2 or 5).
C     CJ       = Scalar parameter defining iteration matrix.
C     H        = Current stepsize in integration.
C     IER      = Variable which is .NE. 0 if iteration matrix
C                is singular, and 0 otherwise.
C     EWT      = Vector of error weights for computing norms.
C     E        = Work space (temporary) of length NEQ.
C     WM       = Real work space for matrices.  On output
C                it contains the LU decomposition
C                of the iteration matrix.
C     IWM      = Integer work space containing
C                matrix information.
C     RES      = External user-supplied subroutine
C                to evaluate the residual.  See RES description
C                in DDASPK prologue.
C     IRES     = Flag which is equal to zero if no illegal values
C                in RES, and less than zero otherwise.  (If IRES
C                is less than zero, the matrix was not completed).
C                In this case (if IRES .LT. 0), then IER = 0.
C     UROUND   = The unit roundoff error of the machine being used.
C     JACD     = Name of the external user-supplied routine
C                to evaluate the iteration matrix.  (This routine
C                is only used if IWM(MTYPE) is 1 or 4)
C                See JAC description for the case INFO(12) = 0
C                in DDASPK prologue.
C     RPAR,IPAR= Real and integer parameter arrays that
C                are used for communication between the
C                calling program and external user routines.
C                They are not altered by DMATD.
C-----------------------------------------------------------------------
C***ROUTINES CALLED
C   JACD, RES, DGETRF, DGBTRF
C
C***END PROLOGUE  DMATD
C
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      DIMENSION Y(*),YPRIME(*),DELTA(*),EWT(*),E(*)
      DIMENSION WM(*),IWM(*), RPAR(*),IPAR(*)
      EXTERNAL  RES, JACD
C
      PARAMETER (LML=1, LMU=2, LMTYPE=4, LNRE=12, LNPD=22, LLCIWP=30)
C
      LIPVT = IWM(LLCIWP)
      IER = 0
      MTYPE=IWM(LMTYPE)
      GO TO (100,200,300,400,500),MTYPE
C
C
C     Dense user-supplied matrix.
C
100   LENPD=IWM(LNPD)
      DO 110 I=1,LENPD
110      WM(I)=0.0D0
      CALL JACD(X,Y,YPRIME,WM,CJ,RPAR,IPAR)
      GO TO 230
C
C
C     Dense finite-difference-generated matrix.
C
200   IRES=0
      NROW=0
      SQUR = SQRT(UROUND)
      DO 210 I=1,NEQ
         DEL=SQUR*MAX(ABS(Y(I)),ABS(H*YPRIME(I)),
     *     ABS(1.D0/EWT(I)))
         DEL=SIGN(DEL,H*YPRIME(I))
         DEL=(Y(I)+DEL)-Y(I)
         YSAVE=Y(I)
         YPSAVE=YPRIME(I)
         Y(I)=Y(I)+DEL
         YPRIME(I)=YPRIME(I)+CJ*DEL
         IWM(LNRE)=IWM(LNRE)+1
         CALL RES(X,Y,YPRIME,CJ,E,IRES,RPAR,IPAR)
         IF (IRES .LT. 0) RETURN
         DELINV=1.0D0/DEL
         DO 220 L=1,NEQ
220        WM(NROW+L)=(E(L)-DELTA(L))*DELINV
      NROW=NROW+NEQ
      Y(I)=YSAVE
      YPRIME(I)=YPSAVE
210   CONTINUE
C
C
C     Do dense-matrix LU decomposition on J.
C
230      CALL DGETRF( NEQ, NEQ, WM, NEQ, IWM(LIPVT), IER)
      RETURN
C
C
C     Dummy section for IWM(MTYPE)=3.
C
300   RETURN
C
C
C     Banded user-supplied matrix.
C
400   LENPD=IWM(LNPD)
      DO 410 I=1,LENPD
410      WM(I)=0.0D0
      CALL JACD(X,Y,YPRIME,WM,CJ,RPAR,IPAR)
      MEBAND=2*IWM(LML)+IWM(LMU)+1
      GO TO 550
C
C
C     Banded finite-difference-generated matrix.
C
500   MBAND=IWM(LML)+IWM(LMU)+1
      MBA=MIN0(MBAND,NEQ)
      MEBAND=MBAND+IWM(LML)
      MEB1=MEBAND-1
      MSAVE=(NEQ/MBAND)+1
      ISAVE=IWM(LNPD)
      IPSAVE=ISAVE+MSAVE
      IRES=0
      SQUR=SQRT(UROUND)
      DO 540 J=1,MBA
        DO 510 N=J,NEQ,MBAND
          K= (N-J)/MBAND + 1
          WM(ISAVE+K)=Y(N)
          WM(IPSAVE+K)=YPRIME(N)
          DEL=SQUR*MAX(ABS(Y(N)),ABS(H*YPRIME(N)),
     *      ABS(1.D0/EWT(N)))
          DEL=SIGN(DEL,H*YPRIME(N))
          DEL=(Y(N)+DEL)-Y(N)
          Y(N)=Y(N)+DEL
510       YPRIME(N)=YPRIME(N)+CJ*DEL
        IWM(LNRE)=IWM(LNRE)+1
        CALL RES(X,Y,YPRIME,CJ,E,IRES,RPAR,IPAR)
        IF (IRES .LT. 0) RETURN
        DO 530 N=J,NEQ,MBAND
          K= (N-J)/MBAND + 1
          Y(N)=WM(ISAVE+K)
          YPRIME(N)=WM(IPSAVE+K)
          DEL=SQUR*MAX(ABS(Y(N)),ABS(H*YPRIME(N)),
     *      ABS(1.D0/EWT(N)))
          DEL=SIGN(DEL,H*YPRIME(N))
          DEL=(Y(N)+DEL)-Y(N)
          DELINV=1.0D0/DEL
          I1=MAX0(1,(N-IWM(LMU)))
          I2=MIN0(NEQ,(N+IWM(LML)))
          II=N*MEB1-IWM(LML)
          DO 520 I=I1,I2
520         WM(II+I)=(E(I)-DELTA(I))*DELINV
530     CONTINUE
540   CONTINUE
C
C
C     Do LU decomposition of banded J.
C
550   CALL DGBTRF(NEQ, NEQ, IWM(LML), IWM(LMU), WM, MEBAND,
     *     IWM(LIPVT), IER)
      RETURN
C
C------END OF SUBROUTINE DMATD------------------------------------------
      END

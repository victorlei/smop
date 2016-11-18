      subroutine xgammainc (a, x, result)

c -- jwe, based on DGAMIT.
c
c -- Do a better job than dgami for large values of x.

      double precision a, x, result
      intrinsic exp, log, sqrt, sign, aint
      external dgami, dlngam, d9lgit, d9lgic, d9gmit

C     external dgamr
C     DOUBLE PRECISION DGAMR

      DOUBLE PRECISION AEPS, AINTA, ALGAP1, ALNEPS, ALNG, ALX,
     $     BOT, H, SGA, SGNGAM, SQEPS, T, D1MACH, D9GMIT,
     $     D9LGIC, D9LGIT, DLNGAM, DGAMI

      LOGICAL FIRST

      SAVE ALNEPS, SQEPS, BOT, FIRST

      DATA FIRST /.TRUE./

      if (x .eq. 0.0d0) then

        if (a .eq. 0.0d0) then
          result = 1.0d0
        else
          result = 0.0d0
        endif

      else

      IF (FIRST) THEN
         ALNEPS = -LOG (D1MACH(3))
         SQEPS = SQRT(D1MACH(4))
         BOT = LOG (D1MACH(1))
      ENDIF
      FIRST = .FALSE.
C
      IF (X .LT. 0.D0) CALL XERMSG ('SLATEC', 'XGMAINC', 'X IS NEGATIVE'
     +   , 2, 2)
C
      IF (X.NE.0.D0) ALX = LOG (X)
      SGA = 1.0D0
      IF (A.NE.0.D0) SGA = SIGN (1.0D0, A)
      AINTA = AINT (A + 0.5D0*SGA)
      AEPS = A - AINTA
C
C      IF (X.GT.0.D0) GO TO 20
C      DGAMIT = 0.0D0
C      IF (AINTA.GT.0.D0 .OR. AEPS.NE.0.D0) DGAMIT = DGAMR(A+1.0D0)
C      RETURN
C
 20   IF (X.GT.1.D0) GO TO 30
      IF (A.GE.(-0.5D0) .OR. AEPS.NE.0.D0) CALL DLGAMS (A+1.0D0, ALGAP1,
     1  SGNGAM)
C      DGAMIT = D9GMIT (A, X, ALGAP1, SGNGAM, ALX)
      result = exp (a*alx + log (D9GMIT (A, X, ALGAP1, SGNGAM, ALX)))
      RETURN
C
 30   IF (A.LT.X) GO TO 40
      T = D9LGIT (A, X, DLNGAM(A+1.0D0))
      IF (T.LT.BOT) CALL XERCLR
C      DGAMIT = EXP (T)
      result = EXP (a*alx + T)
      RETURN
C
 40   ALNG = D9LGIC (A, X, ALX)
C
C EVALUATE DGAMIT IN TERMS OF LOG (DGAMIC (A, X))
C
      H = 1.0D0
      IF (AEPS.EQ.0.D0 .AND. AINTA.LE.0.D0) GO TO 50
C
      CALL DLGAMS (A+1.0D0, ALGAP1, SGNGAM)
      T = LOG (ABS(A)) + ALNG - ALGAP1
      IF (T.GT.ALNEPS) GO TO 60
C
      IF (T.GT.(-ALNEPS)) H = 1.0D0 - SGA * SGNGAM * EXP(T)
      IF (ABS(H).GT.SQEPS) GO TO 50
C
      CALL XERCLR
      CALL XERMSG ('SLATEC', 'XGMAINC', 'RESULT LT HALF PRECISION', 1,
     +   1)
C
C 50   T = -A*ALX + LOG(ABS(H))
C      IF (T.LT.BOT) CALL XERCLR
C      DGAMIT = SIGN (EXP(T), H)
 50   result = H
      RETURN
C
C 60   T = T - A*ALX
 60   IF (T.LT.BOT) CALL XERCLR
      result = -SGA * SGNGAM * EXP(T)
      RETURN

      endif
      return
      end

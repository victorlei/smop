      subroutine xsgammainc (a, x, result)

c -- jwe, based on GAMIT.
c
c -- Do a better job than gami for large values of x.

      real a, x, result
      intrinsic exp, log, sqrt, sign, aint
      external gami, alngam, r9lgit, r9lgic, r9gmit

C     external gamr
C     real GAMR

      REAL AEPS, AINTA, ALGAP1, ALNEPS, ALNG, ALX,
     $     BOT, H, SGA, SGNGAM, SQEPS, T, R1MACH, R9GMIT,
     $     R9LGIC, R9LGIT, ALNGAM, GAMI

      LOGICAL FIRST

      SAVE ALNEPS, SQEPS, BOT, FIRST

      DATA FIRST /.TRUE./

      if (x .eq. 0.0e0) then

        if (a .eq. 0.0e0) then
          result = 1.0e0
        else
          result = 0.0e0
        endif

      else

      IF (FIRST) THEN
         ALNEPS = -LOG (R1MACH(3))
         SQEPS = SQRT(R1MACH(4))
         BOT = LOG (R1MACH(1))
      ENDIF
      FIRST = .FALSE.
C
      IF (X .LT. 0.E0) CALL XERMSG ('SLATEC', 'XGMAINC', 'X IS NEGATIVE'
     +   , 2, 2)
C
      IF (X.NE.0.E0) ALX = LOG (X)
      SGA = 1.0E0
      IF (A.NE.0.E0) SGA = SIGN (1.0E0, A)
      AINTA = AINT (A + 0.5E0*SGA)
      AEPS = A - AINTA
C
C      IF (X.GT.0.E0) GO TO 20
C      GAMIT = 0.0E0
C      IF (AINTA.GT.0.E0 .OR. AEPS.NE.0.E0) GAMIT = GAMR(A+1.0E0)
C      RETURN
C
 20   IF (X.GT.1.E0) GO TO 30
      IF (A.GE.(-0.5E0) .OR. AEPS.NE.0.E0) CALL ALGAMS (A+1.0E0, ALGAP1,
     1  SGNGAM)
C      GAMIT = R9GMIT (A, X, ALGAP1, SGNGAM, ALX)
      result = exp (a*alx + log (R9GMIT (A, X, ALGAP1, SGNGAM, ALX)))
      RETURN
C
 30   IF (A.LT.X) GO TO 40
      T = R9LGIT (A, X, ALNGAM(A+1.0E0))
      IF (T.LT.BOT) CALL XERCLR
C      GAMIT = EXP (T)
      result = EXP (a*alx + T)
      RETURN
C
 40   ALNG = R9LGIC (A, X, ALX)
C
C EVALUATE GAMIT IN TERMS OF LOG (DGAMIC (A, X))
C
      H = 1.0E0
      IF (AEPS.EQ.0.E0 .AND. AINTA.LE.0.E0) GO TO 50
C
      CALL ALGAMS (A+1.0E0, ALGAP1, SGNGAM)
      T = LOG (ABS(A)) + ALNG - ALGAP1
      IF (T.GT.ALNEPS) GO TO 60
C
      IF (T.GT.(-ALNEPS)) H = 1.0E0 - SGA * SGNGAM * EXP(T)
      IF (ABS(H).GT.SQEPS) GO TO 50
C
      CALL XERCLR
      CALL XERMSG ('SLATEC', 'XGMAINC', 'RESULT LT HALF PRECISION', 1,
     +   1)
C
C 50   T = -A*ALX + LOG(ABS(H))
C      IF (T.LT.BOT) CALL XERCLR
C      GAMIT = SIGN (EXP(T), H)
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

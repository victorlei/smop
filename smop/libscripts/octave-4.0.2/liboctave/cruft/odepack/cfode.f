      SUBROUTINE CFODE (METH, ELCO, TESCO)
CLLL. OPTIMIZE
      INTEGER METH
      INTEGER I, IB, NQ, NQM1, NQP1
      DOUBLE PRECISION ELCO, TESCO
      DOUBLE PRECISION AGAMQ, FNQ, FNQM1, PC, PINT, RAGQ,
     1   RQFAC, RQ1FAC, TSIGN, XPIN
      DIMENSION ELCO(13,12), TESCO(3,12)
C-----------------------------------------------------------------------
C CFODE IS CALLED BY THE INTEGRATOR ROUTINE TO SET COEFFICIENTS
C NEEDED THERE.  THE COEFFICIENTS FOR THE CURRENT METHOD, AS
C GIVEN BY THE VALUE OF METH, ARE SET FOR ALL ORDERS AND SAVED.
C THE MAXIMUM ORDER ASSUMED HERE IS 12 IF METH = 1 AND 5 IF METH = 2.
C (A SMALLER VALUE OF THE MAXIMUM ORDER IS ALSO ALLOWED.)
C CFODE IS CALLED ONCE AT THE BEGINNING OF THE PROBLEM,
C AND IS NOT CALLED AGAIN UNLESS AND UNTIL METH IS CHANGED.
C
C THE ELCO ARRAY CONTAINS THE BASIC METHOD COEFFICIENTS.
C THE COEFFICIENTS EL(I), 1 .LE. I .LE. NQ+1, FOR THE METHOD OF
C ORDER NQ ARE STORED IN ELCO(I,NQ).  THEY ARE GIVEN BY A GENETRATING
C POLYNOMIAL, I.E.,
C     L(X) = EL(1) + EL(2)*X + ... + EL(NQ+1)*X**NQ.
C FOR THE IMPLICIT ADAMS METHODS, L(X) IS GIVEN BY
C     DL/DX = (X+1)*(X+2)*...*(X+NQ-1)/FACTORIAL(NQ-1),    L(-1) = 0.
C FOR THE BDF METHODS, L(X) IS GIVEN BY
C     L(X) = (X+1)*(X+2)* ... *(X+NQ)/K,
C WHERE         K = FACTORIAL(NQ)*(1 + 1/2 + ... + 1/NQ).
C
C THE TESCO ARRAY CONTAINS TEST CONSTANTS USED FOR THE
C LOCAL ERROR TEST AND THE SELECTION OF STEP SIZE AND/OR ORDER.
C AT ORDER NQ, TESCO(K,NQ) IS USED FOR THE SELECTION OF STEP
C SIZE AT ORDER NQ - 1 IF K = 1, AT ORDER NQ IF K = 2, AND AT ORDER
C NQ + 1 IF K = 3.
C-----------------------------------------------------------------------
      DIMENSION PC(12)
C
      GO TO (100, 200), METH
C
 100  ELCO(1,1) = 1.0D0
      ELCO(2,1) = 1.0D0
      TESCO(1,1) = 0.0D0
      TESCO(2,1) = 2.0D0
      TESCO(1,2) = 1.0D0
      TESCO(3,12) = 0.0D0
      PC(1) = 1.0D0
      RQFAC = 1.0D0
      DO 140 NQ = 2,12
C-----------------------------------------------------------------------
C THE PC ARRAY WILL CONTAIN THE COEFFICIENTS OF THE POLYNOMIAL
C     P(X) = (X+1)*(X+2)*...*(X+NQ-1).
C INITIALLY, P(X) = 1.
C-----------------------------------------------------------------------
        RQ1FAC = RQFAC
        RQFAC = RQFAC/DBLE(NQ)
        NQM1 = NQ - 1
        FNQM1 = DBLE(NQM1)
        NQP1 = NQ + 1
C FORM COEFFICIENTS OF P(X)*(X+NQ-1). ----------------------------------
        PC(NQ) = 0.0D0
        DO 110 IB = 1,NQM1
          I = NQP1 - IB
 110      PC(I) = PC(I-1) + FNQM1*PC(I)
        PC(1) = FNQM1*PC(1)
C COMPUTE INTEGRAL, -1 TO 0, OF P(X) AND X*P(X). -----------------------
        PINT = PC(1)
        XPIN = PC(1)/2.0D0
        TSIGN = 1.0D0
        DO 120 I = 2,NQ
          TSIGN = -TSIGN
          PINT = PINT + TSIGN*PC(I)/DBLE(I)
 120      XPIN = XPIN + TSIGN*PC(I)/DBLE(I+1)
C STORE COEFFICIENTS IN ELCO AND TESCO. --------------------------------
        ELCO(1,NQ) = PINT*RQ1FAC
        ELCO(2,NQ) = 1.0D0
        DO 130 I = 2,NQ
 130      ELCO(I+1,NQ) = RQ1FAC*PC(I)/DBLE(I)
        AGAMQ = RQFAC*XPIN
        RAGQ = 1.0D0/AGAMQ
        TESCO(2,NQ) = RAGQ
        IF (NQ .LT. 12) TESCO(1,NQP1) = RAGQ*RQFAC/DBLE(NQP1)
        TESCO(3,NQM1) = RAGQ
 140    CONTINUE
      RETURN
C
 200  PC(1) = 1.0D0
      RQ1FAC = 1.0D0
      DO 230 NQ = 1,5
C-----------------------------------------------------------------------
C THE PC ARRAY WILL CONTAIN THE COEFFICIENTS OF THE POLYNOMIAL
C     P(X) = (X+1)*(X+2)*...*(X+NQ).
C INITIALLY, P(X) = 1.
C-----------------------------------------------------------------------
        FNQ = DBLE(NQ)
        NQP1 = NQ + 1
C FORM COEFFICIENTS OF P(X)*(X+NQ). ------------------------------------
        PC(NQP1) = 0.0D0
        DO 210 IB = 1,NQ
          I = NQ + 2 - IB
 210      PC(I) = PC(I-1) + FNQ*PC(I)
        PC(1) = FNQ*PC(1)
C STORE COEFFICIENTS IN ELCO AND TESCO. --------------------------------
        DO 220 I = 1,NQP1
 220      ELCO(I,NQ) = PC(I)/PC(2)
        ELCO(2,NQ) = 1.0D0
        TESCO(1,NQ) = RQ1FAC
        TESCO(2,NQ) = DBLE(NQP1)/ELCO(1,NQ)
        TESCO(3,NQ) = DBLE(NQ+2)/ELCO(1,NQ)
        RQ1FAC = RQ1FAC/FNQ
 230    CONTINUE
      RETURN
C----------------------- END OF SUBROUTINE CFODE -----------------------
      END

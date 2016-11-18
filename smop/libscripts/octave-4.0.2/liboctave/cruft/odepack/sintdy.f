      SUBROUTINE SINTDY (T, K, YH, NYH, DKY, IFLAG)
C***BEGIN PROLOGUE  SINTDY
C***SUBSIDIARY
C***PURPOSE  Interpolate solution derivatives.
C***TYPE      SINGLE PRECISION (SINTDY-S, DINTDY-D)
C***AUTHOR  Hindmarsh, Alan C., (LLNL)
C***DESCRIPTION
C
C  SINTDY computes interpolated values of the K-th derivative of the
C  dependent variable vector y, and stores it in DKY.  This routine
C  is called within the package with K = 0 and T = TOUT, but may
C  also be called by the user for any K up to the current order.
C  (See detailed instructions in the usage documentation.)
C
C  The computed values in DKY are gotten by interpolation using the
C  Nordsieck history array YH.  This array corresponds uniquely to a
C  vector-valued polynomial of degree NQCUR or less, and DKY is set
C  to the K-th derivative of this polynomial at T.
C  The formula for DKY is:
C               q
C   DKY(i)  =  sum  c(j,K) * (T - tn)**(j-K) * h**(-j) * YH(i,j+1)
C              j=K
C  where  c(j,K) = j*(j-1)*...*(j-K+1), q = NQCUR, tn = TCUR, h = HCUR.
C  The quantities  nq = NQCUR, l = nq+1, N = NEQ, tn, and h are
C  communicated by COMMON.  The above sum is done in reverse order.
C  IFLAG is returned negative if either K or T is out of bounds.
C
C***SEE ALSO  SLSODE
C***ROUTINES CALLED  XERRWV
C***COMMON BLOCKS    SLS001
C***REVISION HISTORY  (YYMMDD)
C   791129  DATE WRITTEN
C   890501  Modified prologue to SLATEC/LDOC format.  (FNF)
C   890503  Minor cosmetic changes.  (FNF)
C   930809  Renamed to allow single/double precision versions. (ACH)
C   010412  Reduced size of Common block /SLS001/. (ACH)
C   031105  Restored 'own' variables to Common block /SLS001/, to
C           enable interrupt/restart feature. (ACH)
C   050427  Corrected roundoff decrement in TP. (ACH)
C***END PROLOGUE  SINTDY
C**End
      INTEGER K, NYH, IFLAG
      REAL T, YH, DKY
      DIMENSION YH(NYH,*), DKY(*)
      INTEGER IOWND, IOWNS,
     1   ICF, IERPJ, IERSL, JCUR, JSTART, KFLAG, L,
     2   LYH, LEWT, LACOR, LSAVF, LWM, LIWM, METH, MITER,
     3   MAXORD, MAXCOR, MSBP, MXNCF, N, NQ, NST, NFE, NJE, NQU
      REAL ROWNS,
     1   CCMAX, EL0, H, HMIN, HMXI, HU, RC, TN, UROUND
      COMMON /SLS001/ ROWNS(209),
     1   CCMAX, EL0, H, HMIN, HMXI, HU, RC, TN, UROUND,
     2   IOWND(6), IOWNS(6),
     3   ICF, IERPJ, IERSL, JCUR, JSTART, KFLAG, L,
     4   LYH, LEWT, LACOR, LSAVF, LWM, LIWM, METH, MITER,
     5   MAXORD, MAXCOR, MSBP, MXNCF, N, NQ, NST, NFE, NJE, NQU
      INTEGER I, IC, J, JB, JB2, JJ, JJ1, JP1
      REAL C, R, S, TP
      CHARACTER*80 MSG
C
C***FIRST EXECUTABLE STATEMENT  SINTDY
      IFLAG = 0
      IF (K .LT. 0 .OR. K .GT. NQ) GO TO 80
      TP = TN - HU -  100.0E0*UROUND*SIGN(ABS(TN) + ABS(HU), HU)
      IF ((T-TP)*(T-TN) .GT. 0.0E0) GO TO 90
C
      S = (T - TN)/H
      IC = 1
      IF (K .EQ. 0) GO TO 15
      JJ1 = L - K
      DO 10 JJ = JJ1,NQ
 10     IC = IC*JJ
 15   C = IC
      DO 20 I = 1,N
 20     DKY(I) = C*YH(I,L)
      IF (K .EQ. NQ) GO TO 55
      JB2 = NQ - K
      DO 50 JB = 1,JB2
        J = NQ - JB
        JP1 = J + 1
        IC = 1
        IF (K .EQ. 0) GO TO 35
        JJ1 = JP1 - K
        DO 30 JJ = JJ1,J
 30       IC = IC*JJ
 35     C = IC
        DO 40 I = 1,N
 40       DKY(I) = C*YH(I,JP1) + S*DKY(I)
 50     CONTINUE
      IF (K .EQ. 0) RETURN
 55   R = H**(-K)
      DO 60 I = 1,N
 60     DKY(I) = R*DKY(I)
      RETURN
C
 80   CALL XERRWD('SINTDY-  K (=I1) illegal      ',
     1     30, 51, 0, 1, K, 0, 0, 0.0E0, 0.0E0)
      IFLAG = -1
      RETURN
 90   CALL XERRWD('SINTDY-  T (=R1) illegal      ',
     1     30, 52, 0, 0, 0, 0, 1, T, 0.0E0)
      CALL XERRWD(
     1   '      T not in interval TCUR - HU (= R1) to TCUR (=R2)      ',
     1    60, 52, 0, 0, 0, 0, 2, TP, TN)
      IFLAG = -2
      RETURN
C----------------------- END OF SUBROUTINE SINTDY ----------------------
      END

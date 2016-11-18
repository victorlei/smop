                    SUMMARY OF ROUTINES IN RANDLIB

0. Base Level Routines to Set and Obtain Values of Seeds

(These should be the only base level routines used by  those who don't
need multiple generators with blocks of numbers.)

C**********************************************************************
C
C      SUBROUTINE SETALL(ISEED1,ISEED2)
C               SET ALL random number generators
C      INTEGER ISEED1, ISEED2
C
C**********************************************************************
C**********************************************************************
C
C     SUBROUTINE GETSD(ISEED1,ISEED2)
C               GET SeeD
C     INTEGER ISEED1, ISEED2
C
C     Returns the value of two integer seeds of the current generator
C     in ISEED1, ISEED2
C
C**********************************************************************

I. Higher Level Routines

C**********************************************************************
C
C     REAL FUNCTION GENBET( A, B )
C               GeNerate BETa random deviate
C     REAL A,B
C
C     Returns a single random deviate from the beta distribution with
C     parameters A and B.  The density of the beta is
C               x^(a-1) * (1-x)^(b-1) / B(a,b) for 0 < x < 1
C
C**********************************************************************
C**********************************************************************
C
C     REAL FUNCTION GENCHI( DF )
C                Generate random value of CHIsquare variable
C     REAL DF
C
C     Generates random deviate from the distribution of a chisquare
C     with DF degrees of freedom random variable.
C
C**********************************************************************
C**********************************************************************
C
C     REAL FUNCTION GENEXP( AV )
C                    GENerate EXPonential random deviate
C     REAL AV
C
C     Generates a single random deviate from an exponential
C     distribution with mean AV.
C
C**********************************************************************
C**********************************************************************
C
C     REAL FUNCTION GENF( DFN, DFD )
C                GENerate random deviate from the F distribution
C     REAL DFN, DFD
C
C     Generates a random deviate from the F (variance ratio)
C     distribution with DFN degrees of freedom in the numerator
C     and DFD degrees of freedom in the denominator.
C
C**********************************************************************
C**********************************************************************
C
C     REAL FUNCTION GENGAM( A, R )
C           GENerates random deviates from GAMma distribution
C     REAL A, R
C
C     Generates random deviates from the gamma distribution whose
C     density is
C          (A**R)/Gamma(R) * X**(R-1) * Exp(-A*X)
C
C**********************************************************************
C**********************************************************************
C
C     SUBROUTINE GENMN(PARM,X,WORK)
C              GENerate Multivariate Normal random deviate
C     REAL PARM(*), X(*), WORK(*)
C
C     PARM is set by SETGMN which must be called prior to GENMN.  The
C     generated deviates are placed in X.  WORK is a work array of the
C     same size as X.
C
C**********************************************************************
C**********************************************************************
C
C     SUBROUTINE GENMUL( N, P, NCAT, IX )
C              GENerate MULtinomial random deviate
C     REAL P(*)
C     INTEGER N, NCAT, IX(*)
C
C     Generates deviates from a Multinomial distribution with NCAT
C     categories.  P specifies the probability of an event in each
C     category. The generated deviates are placed in IX.
C
C**********************************************************************
C**********************************************************************
C
C     REAL FUNCTION GENNCH( DF, XNONC )
C           Generate random value of Noncentral CHIsquare variable
C     REAL DF, XNONC
C
C     Generates random deviate  from the  distribution  of a  noncentral
C     chisquare with DF degrees  of freedom and noncentrality  parameter
C     XNONC.
C
C**********************************************************************
C**********************************************************************
C
C     REAL FUNCTION GENNF( DFN, DFD, XNONC )
C           GENerate random deviate from the Noncentral F distribution
C     REAL DFN, DFD, XNONC
C
C     Generates a random deviate from the  noncentral F (variance ratio)
C     distribution with DFN degrees of freedom in the numerator, and DFD
C     degrees of freedom in the denominator, and noncentrality parameter
C     XNONC.
C
C**********************************************************************
C**********************************************************************
C
C     REAL FUNCTION GENNOR( AV, SD )
C         GENerate random deviate from a NORmal distribution
C     REAL AV, SD
C
C     Generates a single random deviate from a normal distribution
C     with mean, AV, and standard deviation, SD.
C
C**********************************************************************
C**********************************************************************
C
C    SUBROUTINE GENPRM( IARRAY, LARRAY )
C               GENerate random PeRMutation of iarray
C    INTEGER IARRAY(LARRAY), LARRAY
C
C**********************************************************************
C**********************************************************************
C
C     REAL FUNCTION GENUNF( LOW, HIGH )
C               GeNerate Uniform Real between LOW and HIGH
C     REAL LOW, HIGH
C
C**********************************************************************
C**********************************************************************
C
C     INTEGER FUNCTION IGNBIN( N, P )
C                    GENerate BINomial random deviate
C     INTEGER N
C     REAL P
C
C     Returns a single random deviate from a binomial
C     distribution whose number of trials is N and whose
C     probability of an event in each trial is P.
C
C**********************************************************************
C**********************************************************************
C
C     INTEGER FUNCTION IGNNBN( N, P )
C               GENerate Negative BiNomial random deviate
C     INTEGER N
C     REAL P
C
C     Returns a single random deviate from a negative binomial
C     distribution with number of events N and whose
C     probability of an event in each trial is P.
C
C**********************************************************************
C**********************************************************************
C
C     INTEGER FUNCTION IGNPOI( AV )
C                    GENerate POIsson random deviate
C     REAL AV
C
C     Generates a single random deviate from a Poisson
C     distribution with mean AV.
C
C**********************************************************************
C**********************************************************************
C
C     INTEGER FUNCTION IGNUIN( LOW, HIGH )
C               GeNerate Uniform INteger
C     INTEGER LOW, HIGH
C
C     Generates an integer uniformly distributed between LOW and HIGH.
C
C**********************************************************************
C**********************************************************************
C
C     SUBROUTINE PHRTSD( PHRASE, SEED1, SEED2 )
C               PHRase To SeeDs
C     CHARACTER*(*) PHRASE
C     INTEGER SEED1, SEED2
C
C     Uses a phrase (character string) to generate two seeds for the RGN
C     random number generator.
C
C**********************************************************************
C**********************************************************************
C
C     REAL FUNCTION RANF()
C                RANDom number generator as a Function
C
C     Returns a random floating point number from a uniform distribution
C     over 0 - 1 (endpoints of this interval are not returned) using the
C     current generator
C
C**********************************************************************
C**********************************************************************
C
C     SUBROUTINE SETGMN( MEANV, COVM, LDCOVM, P, PARM)
C            SET Generate Multivariate Normal random deviate
C     INTEGER LDCOVM, P
C     REAL MEANV(P), COVM(LDCOVM,P), PARM(P*(P+3)/2 + 1)
C
C     P is the length of normal vectors to be generated, MEANV
C     is the vector of their means and COVM(1:P,1:P) is their variance
C     covariance matrix.  LDCOVM is the leading actual dimension of
C     COVM, which this routine needs to know although only the
C     (1:P,1:P) slice of COVM is used.
C     Places information necessary to generate the deviates in PARM.
C
C**********************************************************************

II. Uniform Generator and Associated Routines


      A. SETTING THE SEED OF ALL GENERATORS

C**********************************************************************
C
C      SUBROUTINE SETALL(ISEED1,ISEED2)
C               SET ALL random number generators
C      INTEGER ISEED1, ISEED2
C
C**********************************************************************

      B. OBTAINING RANDOM NUMBERS

C**********************************************************************
C
C     INTEGER FUNCTION IGNLGI()
C               GeNerate LarGe Integer
C
C     Returns a random integer following a uniform distribution over
C     (1, 2147483562) using the current generator.
C
C**********************************************************************

C**********************************************************************
C
C     REAL FUNCTION RANF()
C                RANDom number generator as a Function
C
C     Returns a random floating point number from a uniform distribution
C     over 0 - 1 (endpoints of this interval are not returned) using the
C     current generator
C
C**********************************************************************

      C. SETTING AND OBTAINING THE NUMBER OF THE CURRENT GENERATOR

C**********************************************************************
C
C     SUBROUTINE SETCGN( G )
C                      Set GeNerator
C     INTEGER G
C
C     Sets  the  current  generator to G. All references to a generator
C     are to the current generator.
C
C**********************************************************************

C**********************************************************************
C
C      SUBROUTINE GETCGN(G)
C               GET Current GeNerator
C      INTEGER G
C
C      Returns in G the number of the current random number generator
C
C**********************************************************************

      D. OBTAINING OR CHANGING SEEDS IN CURRENT GENERATOR

C**********************************************************************
C
C     SUBROUTINE ADVNST(K)
C               ADV-a-N-ce ST-ate
C     INTEGER K
C
C     Advances the state  of  the current  generator  by 2^K values  and
C     resets the initial seed to that value.
C
C**********************************************************************

C**********************************************************************
C
C     SUBROUTINE GETSD(ISEED1,ISEED2)
C               GET SeeD
C     INTEGER ISEED1, ISEED2
C
C     Returns the value of two integer seeds of the current generator
C     in ISEED1, ISEED2
C
C**********************************************************************

C**********************************************************************
C
C     SUBROUTINE INITGN(ISDTYP)
C          INIT-ialize current G-e-N-erator
C
C     INTEGER ISDTYP    The state to which the generator is to be set
C          ISDTYP = -1  => sets the seeds to their initial value
C          ISDTYP =  0  => sets the seeds to the first value of
C                          the current block
C          ISDTYP =  1  => sets the seeds to the first value of
C                          the next block
C
C**********************************************************************

C**********************************************************************
C
C     SUBROUTINE SETSD(ISEED1,ISEED2)
C               SET S-ee-D of current generator
C
C     Resets the initial  seed of  the current  generator to  ISEED1 and
C     ISEED2. The seeds of the other generators remain unchanged.
C
C**********************************************************************

      E. MISCELLANY

C**********************************************************************
C
C     INTEGER FUNCTION MLTMOD(A,S,M)
C                    Returns (A*S) MOD M
C     INTEGER A, S, M
C
C**********************************************************************

C**********************************************************************
C
C      SUBROUTINE SETANT(QVALUE)
C               SET ANTithetic
C      LOGICAL QVALUE
C
C     Sets whether the current generator produces antithetic values.  If
C     X   is  the value  normally returned  from  a uniform [0,1] random
C     number generator then 1  - X is the antithetic  value. If X is the
C     value  normally  returned  from a   uniform  [0,N]  random  number
C     generator then N - 1 - X is the antithetic value.
C
C     All generators are initialized to NOT generate antithetic values.
C
C**********************************************************************







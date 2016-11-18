C Work performed under the auspices of the U.S. Department of Energy
C by Lawrence Livermore National Laboratory under contract number
C W-7405-Eng-48.
C
      SUBROUTINE DDASPK (RES, NEQ, T, Y, YPRIME, TOUT, INFO, RTOL, ATOL,
     *   IDID, RWORK, LRW, IWORK, LIW, RPAR, IPAR, JAC, PSOL)
C
C***BEGIN PROLOGUE  DDASPK
C***DATE WRITTEN   890101   (YYMMDD)
C***REVISION DATE  910624
C***REVISION DATE  920929   (CJ in RES call, RES counter fix.)
C***REVISION DATE  921215   (Warnings on poor iteration performance)
C***REVISION DATE  921216   (NRMAX as optional input)
C***REVISION DATE  930315   (Name change: DDINI to DDINIT)
C***REVISION DATE  940822   (Replaced initial condition calculation)
C***REVISION DATE  941101   (Added linesearch in I.C. calculations)
C***REVISION DATE  941220   (Misc. corrections throughout)
C***REVISION DATE  950125   (Added DINVWT routine)
C***REVISION DATE  950714   (Misc. corrections throughout)
C***REVISION DATE  950802   (Default NRMAX = 5, based on tests.)
C***REVISION DATE  950808   (Optional error test added.)
C***REVISION DATE  950814   (Added I.C. constraints and INFO(14))
C***REVISION DATE  950828   (Various minor corrections.)
C***REVISION DATE  951006   (Corrected WT scaling in DFNRMK.)
C***REVISION DATE  960129   (Corrected RL bug in DLINSD, DLINSK.)
C***REVISION DATE  960301   (Added NONNEG to SAVE statement.)
C***CATEGORY NO.  I1A2
C***KEYWORDS  DIFFERENTIAL/ALGEBRAIC, BACKWARD DIFFERENTIATION FORMULAS,
C             IMPLICIT DIFFERENTIAL SYSTEMS, KRYLOV ITERATION
C***AUTHORS   Linda R. Petzold, Peter N. Brown, Alan C. Hindmarsh, and
C                  Clement W. Ulrich
C             Center for Computational Sciences & Engineering, L-316
C             Lawrence Livermore National Laboratory
C             P.O. Box 808,
C             Livermore, CA 94551
C***PURPOSE  This code solves a system of differential/algebraic
C            equations of the form
C               G(t,y,y') = 0 ,
C            using a combination of Backward Differentiation Formula
C            (BDF) methods and a choice of two linear system solution
C            methods: direct (dense or band) or Krylov (iterative).
C            This version is in double precision.
C-----------------------------------------------------------------------
C***DESCRIPTION
C
C *Usage:
C
C      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
C      INTEGER NEQ, INFO(N), IDID, LRW, LIW, IWORK(LIW), IPAR(*)
C      DOUBLE PRECISION T, Y(*), YPRIME(*), TOUT, RTOL(*), ATOL(*),
C         RWORK(LRW), RPAR(*)
C      EXTERNAL  RES, JAC, PSOL
C
C      CALL DDASPK (RES, NEQ, T, Y, YPRIME, TOUT, INFO, RTOL, ATOL,
C     *   IDID, RWORK, LRW, IWORK, LIW, RPAR, IPAR, JAC, PSOL)
C
C  Quantities which may be altered by the code are:
C     T, Y(*), YPRIME(*), INFO(*), RTOL, ATOL, IDID, RWORK(*), IWORK(*)
C
C
C *Arguments:
C
C  RES:EXT          This is the name of a subroutine which you
C                   provide to define the residual function G(t,y,y')
C                   of the differential/algebraic system.
C
C  NEQ:IN           This is the number of equations in the system.
C
C  T:INOUT          This is the current value of the independent
C                   variable.
C
C  Y(*):INOUT       This array contains the solution components at T.
C
C  YPRIME(*):INOUT  This array contains the derivatives of the solution
C                   components at T.
C
C  TOUT:IN          This is a point at which a solution is desired.
C
C  INFO(N):IN       This is an integer array used to communicate details
C                   of how the solution is to be carried out, such as
C                   tolerance type, matrix structure, step size and
C                   order limits, and choice of nonlinear system method.
C                   N must be at least 20.
C
C  RTOL,ATOL:INOUT  These quantities represent absolute and relative
C                   error tolerances (on local error) which you provide
C                   to indicate how accurately you wish the solution to
C                   be computed.  You may choose them to be both scalars
C                   or else both arrays of length NEQ.
C
C  IDID:OUT         This integer scalar is an indicator reporting what
C                   the code did.  You must monitor this variable to
C                   decide what action to take next.
C
C  RWORK:WORK       A real work array of length LRW which provides the
C                   code with needed storage space.
C
C  LRW:IN           The length of RWORK.
C
C  IWORK:WORK       An integer work array of length LIW which provides
C                   the code with needed storage space.
C
C  LIW:IN           The length of IWORK.
C
C  RPAR,IPAR:IN     These are real and integer parameter arrays which
C                   you can use for communication between your calling
C                   program and the RES, JAC, and PSOL subroutines.
C
C  JAC:EXT          This is the name of a subroutine which you may
C                   provide (optionally) for calculating Jacobian
C                   (partial derivative) data involved in solving linear
C                   systems within DDASPK.
C
C  PSOL:EXT         This is the name of a subroutine which you must
C                   provide for solving linear systems if you selected
C                   a Krylov method.  The purpose of PSOL is to solve
C                   linear systems involving a left preconditioner P.
C
C *Overview
C
C  The DDASPK solver uses the backward differentiation formulas of
C  orders one through five to solve a system of the form G(t,y,y') = 0
C  for y = Y and y' = YPRIME.  Values for Y and YPRIME at the initial
C  time must be given as input.  These values should be consistent,
C  that is, if T, Y, YPRIME are the given initial values, they should
C  satisfy G(T,Y,YPRIME) = 0.  However, if consistent values are not
C  known, in many cases you can have DDASPK solve for them -- see INFO(11).
C  (This and other options are described in more detail below.)
C
C  Normally, DDASPK solves the system from T to TOUT.  It is easy to
C  continue the solution to get results at additional TOUT.  This is
C  the interval mode of operation.  Intermediate results can also be
C  obtained easily by specifying INFO(3).
C
C  On each step taken by DDASPK, a sequence of nonlinear algebraic
C  systems arises.  These are solved by one of two types of
C  methods:
C    * a Newton iteration with a direct method for the linear
C      systems involved (INFO(12) = 0), or
C    * a Newton iteration with a preconditioned Krylov iterative
C      method for the linear systems involved (INFO(12) = 1).
C
C  The direct method choices are dense and band matrix solvers,
C  with either a user-supplied or an internal difference quotient
C  Jacobian matrix, as specified by INFO(5) and INFO(6).
C  In the band case, INFO(6) = 1, you must supply half-bandwidths
C  in IWORK(1) and IWORK(2).
C
C  The Krylov method is the Generalized Minimum Residual (GMRES)
C  method, in either complete or incomplete form, and with
C  scaling and preconditioning.  The method is implemented
C  in an algorithm called SPIGMR.  Certain options in the Krylov
C  method case are specified by INFO(13) and INFO(15).
C
C  If the Krylov method is chosen, you may supply a pair of routines,
C  JAC and PSOL, to apply preconditioning to the linear system.
C  If the system is A*x = b, the matrix is A = dG/dY + CJ*dG/dYPRIME
C  (of order NEQ).  This system can then be preconditioned in the form
C  (P-inverse)*A*x = (P-inverse)*b, with left preconditioner P.
C  (DDASPK does not allow right preconditioning.)
C  Then the Krylov method is applied to this altered, but equivalent,
C  linear system, hopefully with much better performance than without
C  preconditioning.  (In addition, a diagonal scaling matrix based on
C  the tolerances is also introduced into the altered system.)
C
C  The JAC routine evaluates any data needed for solving systems
C  with coefficient matrix P, and PSOL carries out that solution.
C  In any case, in order to improve convergence, you should try to
C  make P approximate the matrix A as much as possible, while keeping
C  the system P*x = b reasonably easy and inexpensive to solve for x,
C  given a vector b.
C
C
C *Description
C
C------INPUT - WHAT TO DO ON THE FIRST CALL TO DDASPK-------------------
C
C
C  The first call of the code is defined to be the start of each new
C  problem.  Read through the descriptions of all the following items,
C  provide sufficient storage space for designated arrays, set
C  appropriate variables for the initialization of the problem, and
C  give information about how you want the problem to be solved.
C
C
C  RES -- Provide a subroutine of the form
C
C             SUBROUTINE RES (T, Y, YPRIME, CJ, DELTA, IRES, RPAR, IPAR)
C
C         to define the system of differential/algebraic
C         equations which is to be solved. For the given values
C         of T, Y and YPRIME, the subroutine should return
C         the residual of the differential/algebraic system
C             DELTA = G(T,Y,YPRIME)
C         DELTA is a vector of length NEQ which is output from RES.
C
C         Subroutine RES must not alter T, Y, YPRIME, or CJ.
C         You must declare the name RES in an EXTERNAL
C         statement in your program that calls DDASPK.
C         You must dimension Y, YPRIME, and DELTA in RES.
C
C         The input argument CJ can be ignored, or used to rescale
C         constraint equations in the system (see Ref. 2, p. 145).
C         Note: In this respect, DDASPK is not downward-compatible
C         with DDASSL, which does not have the RES argument CJ.
C
C         IRES is an integer flag which is always equal to zero
C         on input.  Subroutine RES should alter IRES only if it
C         encounters an illegal value of Y or a stop condition.
C         Set IRES = -1 if an input value is illegal, and DDASPK
C         will try to solve the problem without getting IRES = -1.
C         If IRES = -2, DDASPK will return control to the calling
C         program with IDID = -11.
C
C         RPAR and IPAR are real and integer parameter arrays which
C         you can use for communication between your calling program
C         and subroutine RES. They are not altered by DDASPK. If you
C         do not need RPAR or IPAR, ignore these parameters by treat-
C         ing them as dummy arguments. If you do choose to use them,
C         dimension them in your calling program and in RES as arrays
C         of appropriate length.
C
C  NEQ -- Set it to the number of equations in the system (NEQ .GE. 1).
C
C  T -- Set it to the initial point of the integration. (T must be
C       a variable.)
C
C  Y(*) -- Set this array to the initial values of the NEQ solution
C          components at the initial point.  You must dimension Y of
C          length at least NEQ in your calling program.
C
C  YPRIME(*) -- Set this array to the initial values of the NEQ first
C               derivatives of the solution components at the initial
C               point.  You must dimension YPRIME at least NEQ in your
C               calling program.
C
C  TOUT - Set it to the first point at which a solution is desired.
C         You cannot take TOUT = T.  Integration either forward in T
C         (TOUT .GT. T) or backward in T (TOUT .LT. T) is permitted.
C
C         The code advances the solution from T to TOUT using step
C         sizes which are automatically selected so as to achieve the
C         desired accuracy.  If you wish, the code will return with the
C         solution and its derivative at intermediate steps (the
C         intermediate-output mode) so that you can monitor them,
C         but you still must provide TOUT in accord with the basic
C         aim of the code.
C
C         The first step taken by the code is a critical one because
C         it must reflect how fast the solution changes near the
C         initial point.  The code automatically selects an initial
C         step size which is practically always suitable for the
C         problem.  By using the fact that the code will not step past
C         TOUT in the first step, you could, if necessary, restrict the
C         length of the initial step.
C
C         For some problems it may not be permissible to integrate
C         past a point TSTOP, because a discontinuity occurs there
C         or the solution or its derivative is not defined beyond
C         TSTOP.  When you have declared a TSTOP point (see INFO(4)
C         and RWORK(1)), you have told the code not to integrate past
C         TSTOP.  In this case any tout beyond TSTOP is invalid input.
C
C  INFO(*) - Use the INFO array to give the code more details about
C            how you want your problem solved.  This array should be
C            dimensioned of length 20, though DDASPK uses only the
C            first 15 entries.  You must respond to all of the following
C            items, which are arranged as questions.  The simplest use
C            of DDASPK corresponds to setting all entries of INFO to 0.
C
C       INFO(1) - This parameter enables the code to initialize itself.
C              You must set it to indicate the start of every new
C              problem.
C
C          **** Is this the first call for this problem ...
C                yes - set INFO(1) = 0
C                 no - not applicable here.
C                      See below for continuation calls.  ****
C
C       INFO(2) - How much accuracy you want of your solution
C              is specified by the error tolerances RTOL and ATOL.
C              The simplest use is to take them both to be scalars.
C              To obtain more flexibility, they can both be arrays.
C              The code must be told your choice.
C
C          **** Are both error tolerances RTOL, ATOL scalars ...
C                yes - set INFO(2) = 0
C                      and input scalars for both RTOL and ATOL
C                 no - set INFO(2) = 1
C                      and input arrays for both RTOL and ATOL ****
C
C       INFO(3) - The code integrates from T in the direction of TOUT
C              by steps.  If you wish, it will return the computed
C              solution and derivative at the next intermediate step
C              (the intermediate-output mode) or TOUT, whichever comes
C              first.  This is a good way to proceed if you want to
C              see the behavior of the solution.  If you must have
C              solutions at a great many specific TOUT points, this
C              code will compute them efficiently.
C
C          **** Do you want the solution only at
C               TOUT (and not at the next intermediate step) ...
C                yes - set INFO(3) = 0
C                 no - set INFO(3) = 1 ****
C
C       INFO(4) - To handle solutions at a great many specific
C              values TOUT efficiently, this code may integrate past
C              TOUT and interpolate to obtain the result at TOUT.
C              Sometimes it is not possible to integrate beyond some
C              point TSTOP because the equation changes there or it is
C              not defined past TSTOP.  Then you must tell the code
C              this stop condition.
C
C           **** Can the integration be carried out without any
C                restrictions on the independent variable T ...
C                 yes - set INFO(4) = 0
C                  no - set INFO(4) = 1
C                       and define the stopping point TSTOP by
C                       setting RWORK(1) = TSTOP ****
C
C       INFO(5) - used only when INFO(12) = 0 (direct methods).
C              To solve differential/algebraic systems you may wish
C              to use a matrix of partial derivatives of the
C              system of differential equations.  If you do not
C              provide a subroutine to evaluate it analytically (see
C              description of the item JAC in the call list), it will
C              be approximated by numerical differencing in this code.
C              Although it is less trouble for you to have the code
C              compute partial derivatives by numerical differencing,
C              the solution will be more reliable if you provide the
C              derivatives via JAC.  Usually numerical differencing is
C              more costly than evaluating derivatives in JAC, but
C              sometimes it is not - this depends on your problem.
C
C           **** Do you want the code to evaluate the partial deriv-
C                atives automatically by numerical differences ...
C                 yes - set INFO(5) = 0
C                  no - set INFO(5) = 1
C                       and provide subroutine JAC for evaluating the
C                       matrix of partial derivatives ****
C
C       INFO(6) - used only when INFO(12) = 0 (direct methods).
C              DDASPK will perform much better if the matrix of
C              partial derivatives, dG/dY + CJ*dG/dYPRIME (here CJ is
C              a scalar determined by DDASPK), is banded and the code
C              is told this.  In this case, the storage needed will be
C              greatly reduced, numerical differencing will be performed
C              much cheaper, and a number of important algorithms will
C              execute much faster.  The differential equation is said
C              to have half-bandwidths ML (lower) and MU (upper) if
C              equation i involves only unknowns Y(j) with
C                             i-ML .le. j .le. i+MU .
C              For all i=1,2,...,NEQ.  Thus, ML and MU are the widths
C              of the lower and upper parts of the band, respectively,
C              with the main diagonal being excluded.  If you do not
C              indicate that the equation has a banded matrix of partial
C              derivatives the code works with a full matrix of NEQ**2
C              elements (stored in the conventional way).  Computations
C              with banded matrices cost less time and storage than with
C              full matrices if  2*ML+MU .lt. NEQ.  If you tell the
C              code that the matrix of partial derivatives has a banded
C              structure and you want to provide subroutine JAC to
C              compute the partial derivatives, then you must be careful
C              to store the elements of the matrix in the special form
C              indicated in the description of JAC.
C
C          **** Do you want to solve the problem using a full (dense)
C               matrix (and not a special banded structure) ...
C                yes - set INFO(6) = 0
C                 no - set INFO(6) = 1
C                       and provide the lower (ML) and upper (MU)
C                       bandwidths by setting
C                       IWORK(1)=ML
C                       IWORK(2)=MU ****
C
C       INFO(7) - You can specify a maximum (absolute value of)
C              stepsize, so that the code will avoid passing over very
C              large regions.
C
C          ****  Do you want the code to decide on its own the maximum
C                stepsize ...
C                 yes - set INFO(7) = 0
C                  no - set INFO(7) = 1
C                       and define HMAX by setting
C                       RWORK(2) = HMAX ****
C
C       INFO(8) -  Differential/algebraic problems may occasionally
C              suffer from severe scaling difficulties on the first
C              step.  If you know a great deal about the scaling of
C              your problem, you can help to alleviate this problem
C              by specifying an initial stepsize H0.
C
C          ****  Do you want the code to define its own initial
C                stepsize ...
C                 yes - set INFO(8) = 0
C                  no - set INFO(8) = 1
C                       and define H0 by setting
C                       RWORK(3) = H0 ****
C
C       INFO(9) -  If storage is a severe problem, you can save some
C              storage by restricting the maximum method order MAXORD.
C              The default value is 5.  For each order decrease below 5,
C              the code requires NEQ fewer locations, but it is likely
C              to be slower.  In any case, you must have
C              1 .le. MAXORD .le. 5.
C          ****  Do you want the maximum order to default to 5 ...
C                 yes - set INFO(9) = 0
C                  no - set INFO(9) = 1
C                       and define MAXORD by setting
C                       IWORK(3) = MAXORD ****
C
C       INFO(10) - If you know that certain components of the
C              solutions to your equations are always nonnegative
C              (or nonpositive), it may help to set this
C              parameter.  There are three options that are
C              available:
C              1.  To have constraint checking only in the initial
C                  condition calculation.
C              2.  To enforce nonnegativity in Y during the integration.
C              3.  To enforce both options 1 and 2.
C
C              When selecting option 2 or 3, it is probably best to try the
C              code without using this option first, and only use
C              this option if that does not work very well.
C
C          ****  Do you want the code to solve the problem without
C                invoking any special inequality constraints ...
C                 yes - set INFO(10) = 0
C                  no - set INFO(10) = 1 to have option 1 enforced
C                  no - set INFO(10) = 2 to have option 2 enforced
C                  no - set INFO(10) = 3 to have option 3 enforced ****
C
C                  If you have specified INFO(10) = 1 or 3, then you
C                  will also need to identify how each component of Y
C                  in the initial condition calculation is constrained.
C                  You must set:
C                  IWORK(40+I) = +1 if Y(I) must be .GE. 0,
C                  IWORK(40+I) = +2 if Y(I) must be .GT. 0,
C                  IWORK(40+I) = -1 if Y(I) must be .LE. 0, while
C                  IWORK(40+I) = -2 if Y(I) must be .LT. 0, while
C                  IWORK(40+I) =  0 if Y(I) is not constrained.
C
C       INFO(11) - DDASPK normally requires the initial T, Y, and
C              YPRIME to be consistent.  That is, you must have
C              G(T,Y,YPRIME) = 0 at the initial T.  If you do not know
C              the initial conditions precisely, in some cases
C              DDASPK may be able to compute it.
C
C              Denoting the differential variables in Y by Y_d
C              and the algebraic variables by Y_a, DDASPK can solve
C              one of two initialization problems:
C              1.  Given Y_d, calculate Y_a and Y'_d, or
C              2.  Given Y', calculate Y.
C              In either case, initial values for the given
C              components are input, and initial guesses for
C              the unknown components must also be provided as input.
C
C          ****  Are the initial T, Y, YPRIME consistent ...
C
C                 yes - set INFO(11) = 0
C                  no - set INFO(11) = 1 to calculate option 1 above,
C                    or set INFO(11) = 2 to calculate option 2 ****
C
C                  If you have specified INFO(11) = 1, then you
C                  will also need to identify  which are the
C                  differential and which are the algebraic
C                  components (algebraic components are components
C                  whose derivatives do not appear explicitly
C                  in the function G(T,Y,YPRIME)).  You must set:
C                  IWORK(LID+I) = +1 if Y(I) is a differential variable
C                  IWORK(LID+I) = -1 if Y(I) is an algebraic variable,
C                  where LID = 40 if INFO(10) = 0 or 2 and LID = 40+NEQ
C                  if INFO(10) = 1 or 3.
C
C       INFO(12) - Except for the addition of the RES argument CJ,
C              DDASPK by default is downward-compatible with DDASSL,
C              which uses only direct (dense or band) methods to solve
C              the linear systems involved.  You must set INFO(12) to
C              indicate whether you want the direct methods or the
C              Krylov iterative method.
C          ****   Do you want DDASPK to use standard direct methods
C                 (dense or band) or the Krylov (iterative) method ...
C                   direct methods - set INFO(12) = 0.
C                   Krylov method  - set INFO(12) = 1,
C                       and check the settings of INFO(13) and INFO(15).
C
C       INFO(13) - used when INFO(12) = 1 (Krylov methods).
C              DDASPK uses scalars MAXL, KMP, NRMAX, and EPLI for the
C              iterative solution of linear systems.  INFO(13) allows
C              you to override the default values of these parameters.
C              These parameters and their defaults are as follows:
C              MAXL = maximum number of iterations in the SPIGMR
C                 algorithm (MAXL .le. NEQ).  The default is
C                 MAXL = MIN(5,NEQ).
C              KMP = number of vectors on which orthogonalization is
C                 done in the SPIGMR algorithm.  The default is
C                 KMP = MAXL, which corresponds to complete GMRES
C                 iteration, as opposed to the incomplete form.
C              NRMAX = maximum number of restarts of the SPIGMR
C                 algorithm per nonlinear iteration.  The default is
C                 NRMAX = 5.
C              EPLI = convergence test constant in SPIGMR algorithm.
C                 The default is EPLI = 0.05.
C              Note that the length of RWORK depends on both MAXL
C              and KMP.  See the definition of LRW below.
C          ****   Are MAXL, KMP, and EPLI to be given their
C                 default values ...
C                  yes - set INFO(13) = 0
C                   no - set INFO(13) = 1,
C                        and set all of the following:
C                        IWORK(24) = MAXL (1 .le. MAXL .le. NEQ)
C                        IWORK(25) = KMP  (1 .le. KMP .le. MAXL)
C                        IWORK(26) = NRMAX  (NRMAX .ge. 0)
C                        RWORK(10) = EPLI (0 .lt. EPLI .lt. 1.0) ****
C
C        INFO(14) - used with INFO(11) > 0 (initial condition
C               calculation is requested).  In this case, you may
C               request control to be returned to the calling program
C               immediately after the initial condition calculation,
C               before proceeding to the integration of the system
C               (e.g. to examine the computed Y and YPRIME).
C               If this is done, and if the initialization succeeded
C               (IDID = 4), you should reset INFO(11) to 0 for the
C               next call, to prevent the solver from repeating the
C               initialization (and to avoid an infinite loop).
C          ****   Do you want to proceed to the integration after
C                 the initial condition calculation is done ...
C                 yes - set INFO(14) = 0
C                  no - set INFO(14) = 1                        ****
C
C        INFO(15) - used when INFO(12) = 1 (Krylov methods).
C               When using preconditioning in the Krylov method,
C               you must supply a subroutine, PSOL, which solves the
C               associated linear systems using P.
C               The usage of DDASPK is simpler if PSOL can carry out
C               the solution without any prior calculation of data.
C               However, if some partial derivative data is to be
C               calculated in advance and used repeatedly in PSOL,
C               then you must supply a JAC routine to do this,
C               and set INFO(15) to indicate that JAC is to be called
C               for this purpose.  For example, P might be an
C               approximation to a part of the matrix A which can be
C               calculated and LU-factored for repeated solutions of
C               the preconditioner system.  The arrays WP and IWP
C               (described under JAC and PSOL) can be used to
C               communicate data between JAC and PSOL.
C          ****   Does PSOL operate with no prior preparation ...
C                 yes - set INFO(15) = 0 (no JAC routine)
C                  no - set INFO(15) = 1
C                       and supply a JAC routine to evaluate and
C                       preprocess any required Jacobian data.  ****
C
C         INFO(16) - option to exclude algebraic variables from
C               the error test.
C          ****   Do you wish to control errors locally on
C                 all the variables...
C                 yes - set INFO(16) = 0
C                  no - set INFO(16) = 1
C                       If you have specified INFO(16) = 1, then you
C                       will also need to identify  which are the
C                       differential and which are the algebraic
C                       components (algebraic components are components
C                       whose derivatives do not appear explicitly
C                       in the function G(T,Y,YPRIME)).  You must set:
C                       IWORK(LID+I) = +1 if Y(I) is a differential
C                                      variable, and
C                       IWORK(LID+I) = -1 if Y(I) is an algebraic
C                                      variable,
C                       where LID = 40 if INFO(10) = 0 or 2 and
C                       LID = 40 + NEQ if INFO(10) = 1 or 3.
C
C       INFO(17) - used when INFO(11) > 0 (DDASPK is to do an
C              initial condition calculation).
C              DDASPK uses several heuristic control quantities in the
C              initial condition calculation.  They have default values,
C              but can  also be set by the user using INFO(17).
C              These parameters and their defaults are as follows:
C              MXNIT  = maximum number of Newton iterations
C                 per Jacobian or preconditioner evaluation.
C                 The default is:
C                 MXNIT =  5 in the direct case (INFO(12) = 0), and
C                 MXNIT = 15 in the Krylov case (INFO(12) = 1).
C              MXNJ   = maximum number of Jacobian or preconditioner
C                 evaluations.  The default is:
C                 MXNJ = 6 in the direct case (INFO(12) = 0), and
C                 MXNJ = 2 in the Krylov case (INFO(12) = 1).
C              MXNH   = maximum number of values of the artificial
C                 stepsize parameter H to be tried if INFO(11) = 1.
C                 The default is MXNH = 5.
C                 NOTE: the maximum number of Newton iterations
C                 allowed in all is MXNIT*MXNJ*MXNH if INFO(11) = 1,
C                 and MXNIT*MXNJ if INFO(11) = 2.
C              LSOFF  = flag to turn off the linesearch algorithm
C                 (LSOFF = 0 means linesearch is on, LSOFF = 1 means
C                 it is turned off).  The default is LSOFF = 0.
C              STPTOL = minimum scaled step in linesearch algorithm.
C                 The default is STPTOL = (unit roundoff)**(2/3).
C              EPINIT = swing factor in the Newton iteration convergence
C                 test.  The test is applied to the residual vector,
C                 premultiplied by the approximate Jacobian (in the
C                 direct case) or the preconditioner (in the Krylov
C                 case).  For convergence, the weighted RMS norm of
C                 this vector (scaled by the error weights) must be
C                 less than EPINIT*EPCON, where EPCON = .33 is the
C                 analogous test constant used in the time steps.
C                 The default is EPINIT = .01.
C          ****   Are the initial condition heuristic controls to be
C                 given their default values...
C                  yes - set INFO(17) = 0
C                   no - set INFO(17) = 1,
C                        and set all of the following:
C                        IWORK(32) = MXNIT (.GT. 0)
C                        IWORK(33) = MXNJ (.GT. 0)
C                        IWORK(34) = MXNH (.GT. 0)
C                        IWORK(35) = LSOFF ( = 0 or 1)
C                        RWORK(14) = STPTOL (.GT. 0.0)
C                        RWORK(15) = EPINIT (.GT. 0.0)  ****
C
C         INFO(18) - option to get extra printing in initial condition
C                calculation.
C          ****   Do you wish to have extra printing...
C                 no  - set INFO(18) = 0
C                 yes - set INFO(18) = 1 for minimal printing, or
C                       set INFO(18) = 2 for full printing.
C                       If you have specified INFO(18) .ge. 1, data
C                       will be printed with the error handler routines.
C                       To print to a non-default unit number L, include
C                       the line  CALL XSETUN(L)  in your program.  ****
C
C   RTOL, ATOL -- You must assign relative (RTOL) and absolute (ATOL)
C               error tolerances to tell the code how accurately you
C               want the solution to be computed.  They must be defined
C               as variables because the code may change them.
C               you have two choices --
C                     Both RTOL and ATOL are scalars (INFO(2) = 0), or
C                     both RTOL and ATOL are vectors (INFO(2) = 1).
C               In either case all components must be non-negative.
C
C               The tolerances are used by the code in a local error
C               test at each step which requires roughly that
C                        abs(local error in Y(i)) .le. EWT(i) ,
C               where EWT(i) = RTOL*abs(Y(i)) + ATOL is an error weight
C               quantity, for each vector component.
C               (More specifically, a root-mean-square norm is used to
C               measure the size of vectors, and the error test uses the
C               magnitude of the solution at the beginning of the step.)
C
C               The true (global) error is the difference between the
C               true solution of the initial value problem and the
C               computed approximation.  Practically all present day
C               codes, including this one, control the local error at
C               each step and do not even attempt to control the global
C               error directly.
C
C               Usually, but not always, the true accuracy of
C               the computed Y is comparable to the error tolerances.
C               This code will usually, but not always, deliver a more
C               accurate solution if you reduce the tolerances and
C               integrate again.  By comparing two such solutions you
C               can get a fairly reliable idea of the true error in the
C               solution at the larger tolerances.
C
C               Setting ATOL = 0. results in a pure relative error test
C               on that component.  Setting RTOL = 0. results in a pure
C               absolute error test on that component.  A mixed test
C               with non-zero RTOL and ATOL corresponds roughly to a
C               relative error test when the solution component is
C               much bigger than ATOL and to an absolute error test
C               when the solution component is smaller than the
C               threshold ATOL.
C
C               The code will not attempt to compute a solution at an
C               accuracy unreasonable for the machine being used.  It
C               will advise you if you ask for too much accuracy and
C               inform you as to the maximum accuracy it believes
C               possible.
C
C  RWORK(*) -- a real work array, which should be dimensioned in your
C               calling program with a length equal to the value of
C               LRW (or greater).
C
C  LRW -- Set it to the declared length of the RWORK array.  The
C               minimum length depends on the options you have selected,
C               given by a base value plus additional storage as described
C               below.
C
C               If INFO(12) = 0 (standard direct method), the base value is
C               base = 50 + max(MAXORD+4,7)*NEQ.
C               The default value is MAXORD = 5 (see INFO(9)).  With the
C               default MAXORD, base = 50 + 9*NEQ.
C               Additional storage must be added to the base value for
C               any or all of the following options:
C                 if INFO(6) = 0 (dense matrix), add NEQ**2
C                 if INFO(6) = 1 (banded matrix), then
C                    if INFO(5) = 0, add (2*ML+MU+1)*NEQ + 2*(NEQ/(ML+MU+1)+1),
C                    if INFO(5) = 1, add (2*ML+MU+1)*NEQ,
C                 if INFO(16) = 1, add NEQ.
C
C              If INFO(12) = 1 (Krylov method), the base value is
C              base = 50 + (MAXORD+5)*NEQ + (MAXL+3+MIN0(1,MAXL-KMP))*NEQ +
C                      + (MAXL+3)*MAXL + 1 + LENWP.
C              See PSOL for description of LENWP.  The default values are:
C              MAXORD = 5 (see INFO(9)), MAXL = min(5,NEQ) and KMP = MAXL
C              (see INFO(13)).
C              With the default values for MAXORD, MAXL and KMP,
C              base = 91 + 18*NEQ + LENWP.
C              Additional storage must be added to the base value for
C              any or all of the following options:
C                if INFO(16) = 1, add NEQ.
C
C
C  IWORK(*) -- an integer work array, which should be dimensioned in
C              your calling program with a length equal to the value
C              of LIW (or greater).
C
C  LIW -- Set it to the declared length of the IWORK array.  The
C             minimum length depends on the options you have selected,
C             given by a base value plus additional storage as described
C             below.
C
C             If INFO(12) = 0 (standard direct method), the base value is
C             base = 40 + NEQ.
C             IF INFO(10) = 1 or 3, add NEQ to the base value.
C             If INFO(11) = 1 or INFO(16) =1, add NEQ to the base value.
C
C             If INFO(12) = 1 (Krylov method), the base value is
C             base = 40 + LENIWP.
C             See PSOL for description of LENIWP.
C             IF INFO(10) = 1 or 3, add NEQ to the base value.
C             If INFO(11) = 1 or INFO(16) = 1, add NEQ to the base value.
C
C
C  RPAR, IPAR -- These are arrays of double precision and integer type,
C             respectively, which are available for you to use
C             for communication between your program that calls
C             DDASPK and the RES subroutine (and the JAC and PSOL
C             subroutines).  They are not altered by DDASPK.
C             If you do not need RPAR or IPAR, ignore these
C             parameters by treating them as dummy arguments.
C             If you do choose to use them, dimension them in
C             your calling program and in RES (and in JAC and PSOL)
C             as arrays of appropriate length.
C
C  JAC -- This is the name of a routine that you may supply
C         (optionally) that relates to the Jacobian matrix of the
C         nonlinear system that the code must solve at each T step.
C         The role of JAC (and its call sequence) depends on whether
C         a direct (INFO(12) = 0) or Krylov (INFO(12) = 1) method
C         is selected.
C
C         **** INFO(12) = 0 (direct methods):
C           If you are letting the code generate partial derivatives
C           numerically (INFO(5) = 0), then JAC can be absent
C           (or perhaps a dummy routine to satisfy the loader).
C           Otherwise you must supply a JAC routine to compute
C           the matrix A = dG/dY + CJ*dG/dYPRIME.  It must have
C           the form
C
C           SUBROUTINE JAC (T, Y, YPRIME, PD, CJ, RPAR, IPAR)
C
C           The JAC routine must dimension Y, YPRIME, and PD (and RPAR
C           and IPAR if used).  CJ is a scalar which is input to JAC.
C           For the given values of T, Y, and YPRIME, the JAC routine
C           must evaluate the nonzero elements of the matrix A, and
C           store these values in the array PD.  The elements of PD are
C           set to zero before each call to JAC, so that only nonzero
C           elements need to be defined.
C           The way you store the elements into the PD array depends
C           on the structure of the matrix indicated by INFO(6).
C           *** INFO(6) = 0 (full or dense matrix) ***
C               Give PD a first dimension of NEQ.  When you evaluate the
C               nonzero partial derivatives of equation i (i.e. of G(i))
C               with respect to component j (of Y and YPRIME), you must
C               store the element in PD according to
C                  PD(i,j) = dG(i)/dY(j) + CJ*dG(i)/dYPRIME(j).
C           *** INFO(6) = 1 (banded matrix with half-bandwidths ML, MU
C                            as described under INFO(6)) ***
C               Give PD a first dimension of 2*ML+MU+1.  When you
C               evaluate the nonzero partial derivatives of equation i
C               (i.e. of G(i)) with respect to component j (of Y and
C               YPRIME), you must store the element in PD according to
C                  IROW = i - j + ML + MU + 1
C                  PD(IROW,j) = dG(i)/dY(j) + CJ*dG(i)/dYPRIME(j).
C
C          **** INFO(12) = 1 (Krylov method):
C            If you are not calculating Jacobian data in advance for use
C            in PSOL (INFO(15) = 0), JAC can be absent (or perhaps a
C            dummy routine to satisfy the loader).  Otherwise, you may
C            supply a JAC routine to compute and preprocess any parts of
C            of the Jacobian matrix  A = dG/dY + CJ*dG/dYPRIME that are
C            involved in the preconditioner matrix P.
C            It is to have the form
C
C            SUBROUTINE JAC (RES, IRES, NEQ, T, Y, YPRIME, REWT, SAVR,
C                            WK, H, CJ, WP, IWP, IER, RPAR, IPAR)
C
C           The JAC routine must dimension Y, YPRIME, REWT, SAVR, WK,
C           and (if used) WP, IWP, RPAR, and IPAR.
C           The Y, YPRIME, and SAVR arrays contain the current values
C           of Y, YPRIME, and the residual G, respectively.
C           The array WK is work space of length NEQ.
C           H is the step size.  CJ is a scalar, input to JAC, that is
C           normally proportional to 1/H.  REWT is an array of
C           reciprocal error weights, 1/EWT(i), where EWT(i) is
C           RTOL*abs(Y(i)) + ATOL (unless you supplied routine DDAWTS
C           instead), for use in JAC if needed.  For example, if JAC
C           computes difference quotient approximations to partial
C           derivatives, the REWT array may be useful in setting the
C           increments used.  The JAC routine should do any
C           factorization operations called for, in preparation for
C           solving linear systems in PSOL.  The matrix P should
C           be an approximation to the Jacobian,
C           A = dG/dY + CJ*dG/dYPRIME.
C
C           WP and IWP are real and integer work arrays which you may
C           use for communication between your JAC routine and your
C           PSOL routine.  These may be used to store elements of the
C           preconditioner P, or related matrix data (such as factored
C           forms).  They are not altered by DDASPK.
C           If you do not need WP or IWP, ignore these parameters by
C           treating them as dummy arguments.  If you do use them,
C           dimension them appropriately in your JAC and PSOL routines.
C           See the PSOL description for instructions on setting
C           the lengths of WP and IWP.
C
C           On return, JAC should set the error flag IER as follows..
C             IER = 0    if JAC was successful,
C             IER .ne. 0 if JAC was unsuccessful (e.g. if Y or YPRIME
C                        was illegal, or a singular matrix is found).
C           (If IER .ne. 0, a smaller stepsize will be tried.)
C           IER = 0 on entry to JAC, so need be reset only on a failure.
C           If RES is used within JAC, then a nonzero value of IRES will
C           override any nonzero value of IER (see the RES description).
C
C         Regardless of the method type, subroutine JAC must not
C         alter T, Y(*), YPRIME(*), H, CJ, or REWT(*).
C         You must declare the name JAC in an EXTERNAL statement in
C         your program that calls DDASPK.
C
C PSOL --  This is the name of a routine you must supply if you have
C         selected a Krylov method (INFO(12) = 1) with preconditioning.
C         In the direct case (INFO(12) = 0), PSOL can be absent
C         (a dummy routine may have to be supplied to satisfy the
C         loader).  Otherwise, you must provide a PSOL routine to
C         solve linear systems arising from preconditioning.
C         When supplied with INFO(12) = 1, the PSOL routine is to
C         have the form
C
C         SUBROUTINE PSOL (NEQ, T, Y, YPRIME, SAVR, WK, CJ, WGHT,
C                          WP, IWP, B, EPLIN, IER, RPAR, IPAR)
C
C         The PSOL routine must solve linear systems of the form
C         P*x = b where P is the left preconditioner matrix.
C
C         The right-hand side vector b is in the B array on input, and
C         PSOL must return the solution vector x in B.
C         The Y, YPRIME, and SAVR arrays contain the current values
C         of Y, YPRIME, and the residual G, respectively.
C
C         Work space required by JAC and/or PSOL, and space for data to
C         be communicated from JAC to PSOL is made available in the form
C         of arrays WP and IWP, which are parts of the RWORK and IWORK
C         arrays, respectively.  The lengths of these real and integer
C         work spaces WP and IWP must be supplied in LENWP and LENIWP,
C         respectively, as follows..
C           IWORK(27) = LENWP = length of real work space WP
C           IWORK(28) = LENIWP = length of integer work space IWP.
C
C         WK is a work array of length NEQ for use by PSOL.
C         CJ is a scalar, input to PSOL, that is normally proportional
C         to 1/H (H = stepsize).  If the old value of CJ
C         (at the time of the last JAC call) is needed, it must have
C         been saved by JAC in WP.
C
C         WGHT is an array of weights, to be used if PSOL uses an
C         iterative method and performs a convergence test.  (In terms
C         of the argument REWT to JAC, WGHT is REWT/sqrt(NEQ).)
C         If PSOL uses an iterative method, it should use EPLIN
C         (a heuristic parameter) as the bound on the weighted norm of
C         the residual for the computed solution.  Specifically, the
C         residual vector R should satisfy
C              SQRT (SUM ( (R(i)*WGHT(i))**2 ) ) .le. EPLIN
C
C         PSOL must not alter NEQ, T, Y, YPRIME, SAVR, CJ, WGHT, EPLIN.
C
C         On return, PSOL should set the error flag IER as follows..
C           IER = 0 if PSOL was successful,
C           IER .lt. 0 if an unrecoverable error occurred, meaning
C                 control will be passed to the calling routine,
C           IER .gt. 0 if a recoverable error occurred, meaning that
C                 the step will be retried with the same step size
C                 but with a call to JAC to update necessary data,
C                 unless the Jacobian data is current, in which case
C                 the step will be retried with a smaller step size.
C           IER = 0 on entry to PSOL so need be reset only on a failure.
C
C         You must declare the name PSOL in an EXTERNAL statement in
C         your program that calls DDASPK.
C
C
C  OPTIONALLY REPLACEABLE SUBROUTINE:
C
C  DDASPK uses a weighted root-mean-square norm to measure the
C  size of various error vectors.  The weights used in this norm
C  are set in the following subroutine:
C
C    SUBROUTINE DDAWTS (NEQ, IWT, RTOL, ATOL, Y, EWT, RPAR, IPAR)
C    DIMENSION RTOL(*), ATOL(*), Y(*), EWT(*), RPAR(*), IPAR(*)
C
C  A DDAWTS routine has been included with DDASPK which sets the
C  weights according to
C    EWT(I) = RTOL*ABS(Y(I)) + ATOL
C  in the case of scalar tolerances (IWT = 0) or
C    EWT(I) = RTOL(I)*ABS(Y(I)) + ATOL(I)
C  in the case of array tolerances (IWT = 1).  (IWT is INFO(2).)
C  In some special cases, it may be appropriate for you to define
C  your own error weights by writing a subroutine DDAWTS to be
C  called instead of the version supplied.  However, this should
C  be attempted only after careful thought and consideration.
C  If you supply this routine, you may use the tolerances and Y
C  as appropriate, but do not overwrite these variables.  You
C  may also use RPAR and IPAR to communicate data as appropriate.
C  ***Note: Aside from the values of the weights, the choice of
C  norm used in DDASPK (weighted root-mean-square) is not subject
C  to replacement by the user.  In this respect, DDASPK is not
C  downward-compatible with the original DDASSL solver (in which
C  the norm routine was optionally user-replaceable).
C
C
C------OUTPUT - AFTER ANY RETURN FROM DDASPK----------------------------
C
C  The principal aim of the code is to return a computed solution at
C  T = TOUT, although it is also possible to obtain intermediate
C  results along the way.  To find out whether the code achieved its
C  goal or if the integration process was interrupted before the task
C  was completed, you must check the IDID parameter.
C
C
C   T -- The output value of T is the point to which the solution
C        was successfully advanced.
C
C   Y(*) -- contains the computed solution approximation at T.
C
C   YPRIME(*) -- contains the computed derivative approximation at T.
C
C   IDID -- reports what the code did, described as follows:
C
C                     *** TASK COMPLETED ***
C                Reported by positive values of IDID
C
C           IDID = 1 -- a step was successfully taken in the
C                   intermediate-output mode.  The code has not
C                   yet reached TOUT.
C
C           IDID = 2 -- the integration to TSTOP was successfully
C                   completed (T = TSTOP) by stepping exactly to TSTOP.
C
C           IDID = 3 -- the integration to TOUT was successfully
C                   completed (T = TOUT) by stepping past TOUT.
C                   Y(*) and YPRIME(*) are obtained by interpolation.
C
C           IDID = 4 -- the initial condition calculation, with
C                   INFO(11) > 0, was successful, and INFO(14) = 1.
C                   No integration steps were taken, and the solution
C                   is not considered to have been started.
C
C                    *** TASK INTERRUPTED ***
C                Reported by negative values of IDID
C
C           IDID = -1 -- a large amount of work has been expended
C                     (about 500 steps).
C
C           IDID = -2 -- the error tolerances are too stringent.
C
C           IDID = -3 -- the local error test cannot be satisfied
C                     because you specified a zero component in ATOL
C                     and the corresponding computed solution component
C                     is zero.  Thus, a pure relative error test is
C                     impossible for this component.
C
C           IDID = -5 -- there were repeated failures in the evaluation
C                     or processing of the preconditioner (in JAC).
C
C           IDID = -6 -- DDASPK had repeated error test failures on the
C                     last attempted step.
C
C           IDID = -7 -- the nonlinear system solver in the time integration
C                     could not converge.
C
C           IDID = -8 -- the matrix of partial derivatives appears
C                     to be singular (direct method).
C
C           IDID = -9 -- the nonlinear system solver in the time integration
C                     failed to achieve convergence, and there were repeated
C                     error test failures in this step.
C
C           IDID =-10 -- the nonlinear system solver in the time integration
C                     failed to achieve convergence because IRES was equal
C                     to -1.
C
C           IDID =-11 -- IRES = -2 was encountered and control is
C                     being returned to the calling program.
C
C           IDID =-12 -- DDASPK failed to compute the initial Y, YPRIME.
C
C           IDID =-13 -- unrecoverable error encountered inside user's
C                     PSOL routine, and control is being returned to
C                     the calling program.
C
C           IDID =-14 -- the Krylov linear system solver could not
C                     achieve convergence.
C
C           IDID =-15,..,-32 -- Not applicable for this code.
C
C                    *** TASK TERMINATED ***
C                reported by the value of IDID=-33
C
C           IDID = -33 -- the code has encountered trouble from which
C                   it cannot recover.  A message is printed
C                   explaining the trouble and control is returned
C                   to the calling program.  For example, this occurs
C                   when invalid input is detected.
C
C   RTOL, ATOL -- these quantities remain unchanged except when
C               IDID = -2.  In this case, the error tolerances have been
C               increased by the code to values which are estimated to
C               be appropriate for continuing the integration.  However,
C               the reported solution at T was obtained using the input
C               values of RTOL and ATOL.
C
C   RWORK, IWORK -- contain information which is usually of no interest
C               to the user but necessary for subsequent calls.
C               However, you may be interested in the performance data
C               listed below.  These quantities are accessed in RWORK
C               and IWORK but have internal mnemonic names, as follows..
C
C               RWORK(3)--contains H, the step size h to be attempted
C                        on the next step.
C
C               RWORK(4)--contains TN, the current value of the
C                        independent variable, i.e. the farthest point
C                        integration has reached.  This will differ
C                        from T if interpolation has been performed
C                        (IDID = 3).
C
C               RWORK(7)--contains HOLD, the stepsize used on the last
C                        successful step.  If INFO(11) = INFO(14) = 1,
C                        this contains the value of H used in the
C                        initial condition calculation.
C
C               IWORK(7)--contains K, the order of the method to be
C                        attempted on the next step.
C
C               IWORK(8)--contains KOLD, the order of the method used
C                        on the last step.
C
C               IWORK(11)--contains NST, the number of steps (in T)
C                        taken so far.
C
C               IWORK(12)--contains NRE, the number of calls to RES
C                        so far.
C
C               IWORK(13)--contains NJE, the number of calls to JAC so
C                        far (Jacobian or preconditioner evaluations).
C
C               IWORK(14)--contains NETF, the total number of error test
C                        failures so far.
C
C               IWORK(15)--contains NCFN, the total number of nonlinear
C                        convergence failures so far (includes counts
C                        of singular iteration matrix or singular
C                        preconditioners).
C
C               IWORK(16)--contains NCFL, the number of convergence
C                        failures of the linear iteration so far.
C
C               IWORK(17)--contains LENIW, the length of IWORK actually
C                        required.  This is defined on normal returns
C                        and on an illegal input return for
C                        insufficient storage.
C
C               IWORK(18)--contains LENRW, the length of RWORK actually
C                        required.  This is defined on normal returns
C                        and on an illegal input return for
C                        insufficient storage.
C
C               IWORK(19)--contains NNI, the total number of nonlinear
C                        iterations so far (each of which calls a
C                        linear solver).
C
C               IWORK(20)--contains NLI, the total number of linear
C                        (Krylov) iterations so far.
C
C               IWORK(21)--contains NPS, the number of PSOL calls so
C                        far, for preconditioning solve operations or
C                        for solutions with the user-supplied method.
C
C               Note: The various counters in IWORK do not include
C               counts during a call made with INFO(11) > 0 and
C               INFO(14) = 1.
C
C
C------INPUT - WHAT TO DO TO CONTINUE THE INTEGRATION  -----------------
C              (CALLS AFTER THE FIRST)
C
C     This code is organized so that subsequent calls to continue the
C     integration involve little (if any) additional effort on your
C     part.  You must monitor the IDID parameter in order to determine
C     what to do next.
C
C     Recalling that the principal task of the code is to integrate
C     from T to TOUT (the interval mode), usually all you will need
C     to do is specify a new TOUT upon reaching the current TOUT.
C
C     Do not alter any quantity not specifically permitted below.  In
C     particular do not alter NEQ, T, Y(*), YPRIME(*), RWORK(*),
C     IWORK(*), or the differential equation in subroutine RES.  Any
C     such alteration constitutes a new problem and must be treated
C     as such, i.e. you must start afresh.
C
C     You cannot change from array to scalar error control or vice
C     versa (INFO(2)), but you can change the size of the entries of
C     RTOL or ATOL.  Increasing a tolerance makes the equation easier
C     to integrate.  Decreasing a tolerance will make the equation
C     harder to integrate and should generally be avoided.
C
C     You can switch from the intermediate-output mode to the
C     interval mode (INFO(3)) or vice versa at any time.
C
C     If it has been necessary to prevent the integration from going
C     past a point TSTOP (INFO(4), RWORK(1)), keep in mind that the
C     code will not integrate to any TOUT beyond the currently
C     specified TSTOP.  Once TSTOP has been reached, you must change
C     the value of TSTOP or set INFO(4) = 0.  You may change INFO(4)
C     or TSTOP at any time but you must supply the value of TSTOP in
C     RWORK(1) whenever you set INFO(4) = 1.
C
C     Do not change INFO(5), INFO(6), INFO(12-17) or their associated
C     IWORK/RWORK locations unless you are going to restart the code.
C
C                    *** FOLLOWING A COMPLETED TASK ***
C
C     If..
C     IDID = 1, call the code again to continue the integration
C                  another step in the direction of TOUT.
C
C     IDID = 2 or 3, define a new TOUT and call the code again.
C                  TOUT must be different from T.  You cannot change
C                  the direction of integration without restarting.
C
C     IDID = 4, reset INFO(11) = 0 and call the code again to begin
C                  the integration.  (If you leave INFO(11) > 0 and
C                  INFO(14) = 1, you may generate an infinite loop.)
C                  In this situation, the next call to DASPK is
C                  considered to be the first call for the problem,
C                  in that all initializations are done.
C
C                    *** FOLLOWING AN INTERRUPTED TASK ***
C
C     To show the code that you realize the task was interrupted and
C     that you want to continue, you must take appropriate action and
C     set INFO(1) = 1.
C
C     If..
C     IDID = -1, the code has taken about 500 steps.  If you want to
C                  continue, set INFO(1) = 1 and call the code again.
C                  An additional 500 steps will be allowed.
C
C
C     IDID = -2, the error tolerances RTOL, ATOL have been increased
C                  to values the code estimates appropriate for
C                  continuing.  You may want to change them yourself.
C                  If you are sure you want to continue with relaxed
C                  error tolerances, set INFO(1) = 1 and call the code
C                  again.
C
C     IDID = -3, a solution component is zero and you set the
C                  corresponding component of ATOL to zero.  If you
C                  are sure you want to continue, you must first alter
C                  the error criterion to use positive values of ATOL
C                  for those components corresponding to zero solution
C                  components, then set INFO(1) = 1 and call the code
C                  again.
C
C     IDID = -4  --- cannot occur with this code.
C
C     IDID = -5, your JAC routine failed with the Krylov method.  Check
C                  for errors in JAC and restart the integration.
C
C     IDID = -6, repeated error test failures occurred on the last
C                  attempted step in DDASPK.  A singularity in the
C                  solution may be present.  If you are absolutely
C                  certain you want to continue, you should restart
C                  the integration.  (Provide initial values of Y and
C                  YPRIME which are consistent.)
C
C     IDID = -7, repeated convergence test failures occurred on the last
C                  attempted step in DDASPK.  An inaccurate or ill-
C                  conditioned Jacobian or preconditioner may be the
C                  problem.  If you are absolutely certain you want
C                  to continue, you should restart the integration.
C
C
C     IDID = -8, the matrix of partial derivatives is singular, with
C                  the use of direct methods.  Some of your equations
C                  may be redundant.  DDASPK cannot solve the problem
C                  as stated.  It is possible that the redundant
C                  equations could be removed, and then DDASPK could
C                  solve the problem.  It is also possible that a
C                  solution to your problem either does not exist
C                  or is not unique.
C
C     IDID = -9, DDASPK had multiple convergence test failures, preceded
C                  by multiple error test failures, on the last
C                  attempted step.  It is possible that your problem is
C                  ill-posed and cannot be solved using this code.  Or,
C                  there may be a discontinuity or a singularity in the
C                  solution.  If you are absolutely certain you want to
C                  continue, you should restart the integration.
C
C     IDID = -10, DDASPK had multiple convergence test failures
C                  because IRES was equal to -1.  If you are
C                  absolutely certain you want to continue, you
C                  should restart the integration.
C
C     IDID = -11, there was an unrecoverable error (IRES = -2) from RES
C                  inside the nonlinear system solver.  Determine the
C                  cause before trying again.
C
C     IDID = -12, DDASPK failed to compute the initial Y and YPRIME
C                  vectors.  This could happen because the initial
C                  approximation to Y or YPRIME was not very good, or
C                  because no consistent values of these vectors exist.
C                  The problem could also be caused by an inaccurate or
C                  singular iteration matrix, or a poor preconditioner.
C
C     IDID = -13, there was an unrecoverable error encountered inside
C                  your PSOL routine.  Determine the cause before
C                  trying again.
C
C     IDID = -14, the Krylov linear system solver failed to achieve
C                  convergence.  This may be due to ill-conditioning
C                  in the iteration matrix, or a singularity in the
C                  preconditioner (if one is being used).
C                  Another possibility is that there is a better
C                  choice of Krylov parameters (see INFO(13)).
C                  Possibly the failure is caused by redundant equations
C                  in the system, or by inconsistent equations.
C                  In that case, reformulate the system to make it
C                  consistent and non-redundant.
C
C     IDID = -15,..,-32 --- Cannot occur with this code.
C
C                       *** FOLLOWING A TERMINATED TASK ***
C
C     If IDID = -33, you cannot continue the solution of this problem.
C                  An attempt to do so will result in your run being
C                  terminated.
C
C  ---------------------------------------------------------------------
C
C***REFERENCES
C  1.  L. R. Petzold, A Description of DASSL: A Differential/Algebraic
C      System Solver, in Scientific Computing, R. S. Stepleman et al.
C      (Eds.), North-Holland, Amsterdam, 1983, pp. 65-68.
C  2.  K. E. Brenan, S. L. Campbell, and L. R. Petzold, Numerical
C      Solution of Initial-Value Problems in Differential-Algebraic
C      Equations, Elsevier, New York, 1989.
C  3.  P. N. Brown and A. C. Hindmarsh, Reduced Storage Matrix Methods
C      in Stiff ODE Systems, J. Applied Mathematics and Computation,
C      31 (1989), pp. 40-91.
C  4.  P. N. Brown, A. C. Hindmarsh, and L. R. Petzold, Using Krylov
C      Methods in the Solution of Large-Scale Differential-Algebraic
C      Systems, SIAM J. Sci. Comp., 15 (1994), pp. 1467-1488.
C  5.  P. N. Brown, A. C. Hindmarsh, and L. R. Petzold, Consistent
C      Initial Condition Calculation for Differential-Algebraic
C      Systems, LLNL Report UCRL-JC-122175, August 1995; submitted to
C      SIAM J. Sci. Comp.
C
C***ROUTINES CALLED
C
C   The following are all the subordinate routines used by DDASPK.
C
C   DDASIC computes consistent initial conditions.
C   DYYPNW updates Y and YPRIME in linesearch for initial condition
C          calculation.
C   DDSTP  carries out one step of the integration.
C   DCNSTR/DCNST0 check the current solution for constraint violations.
C   DDAWTS sets error weight quantities.
C   DINVWT tests and inverts the error weights.
C   DDATRP performs interpolation to get an output solution.
C   DDWNRM computes the weighted root-mean-square norm of a vector.
C   D1MACH provides the unit roundoff of the computer.
C   XERRWD/XSETF/XSETUN/IXSAV is a package to handle error messages.
C   DDASID nonlinear equation driver to initialize Y and YPRIME using
C          direct linear system solver methods.  Interfaces to Newton
C          solver (direct case).
C   DNSID  solves the nonlinear system for unknown initial values by
C          modified Newton iteration and direct linear system methods.
C   DLINSD carries out linesearch algorithm for initial condition
C          calculation (direct case).
C   DFNRMD calculates weighted norm of preconditioned residual in
C          initial condition calculation (direct case).
C   DNEDD  nonlinear equation driver for direct linear system solver
C          methods.  Interfaces to Newton solver (direct case).
C   DMATD  assembles the iteration matrix (direct case).
C   DNSD   solves the associated nonlinear system by modified
C          Newton iteration and direct linear system methods.
C   DSLVD  interfaces to linear system solver (direct case).
C   DDASIK nonlinear equation driver to initialize Y and YPRIME using
C          Krylov iterative linear system methods.  Interfaces to
C          Newton solver (Krylov case).
C   DNSIK  solves the nonlinear system for unknown initial values by
C          Newton iteration and Krylov iterative linear system methods.
C   DLINSK carries out linesearch algorithm for initial condition
C          calculation (Krylov case).
C   DFNRMK calculates weighted norm of preconditioned residual in
C          initial condition calculation (Krylov case).
C   DNEDK  nonlinear equation driver for iterative linear system solver
C          methods.  Interfaces to Newton solver (Krylov case).
C   DNSK   solves the associated nonlinear system by Inexact Newton
C          iteration and (linear) Krylov iteration.
C   DSLVK  interfaces to linear system solver (Krylov case).
C   DSPIGM solves a linear system by SPIGMR algorithm.
C   DATV   computes matrix-vector product in Krylov algorithm.
C   DORTH  performs orthogonalization of Krylov basis vectors.
C   DHEQR  performs QR factorization of Hessenberg matrix.
C   DHELS  finds least-squares solution of Hessenberg linear system.
C   DGETRF, DGETRS, DGBTRF, DGBTRS are LAPACK routines for solving
C          linear systems (dense or band direct methods).
C   DAXPY, DCOPY, DDOT, DNRM2, DSCAL are Basic Linear Algebra (BLAS)
C          routines.
C
C The routines called directly by DDASPK are:
C   DCNST0, DDAWTS, DINVWT, D1MACH, DDWNRM, DDASIC, DDATRP, DDSTP,
C   XERRWD
C
C***END PROLOGUE DDASPK
C
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      LOGICAL DONE, LAVL, LCFN, LCFL, LWARN
      DIMENSION Y(*),YPRIME(*)
      DIMENSION INFO(20)
      DIMENSION RWORK(LRW),IWORK(LIW)
      DIMENSION RTOL(*),ATOL(*)
      DIMENSION RPAR(*),IPAR(*)
      CHARACTER MSG*80
      EXTERNAL  RES, JAC, PSOL, DDASID, DDASIK, DNEDD, DNEDK
C
C     Set pointers into IWORK.
C
      PARAMETER (LML=1, LMU=2, LMTYPE=4,
     *   LIWM=1, LMXORD=3, LJCALC=5, LPHASE=6, LK=7, LKOLD=8,
     *   LNS=9, LNSTL=10, LNST=11, LNRE=12, LNJE=13, LETF=14, LNCFN=15,
     *   LNCFL=16, LNIW=17, LNRW=18, LNNI=19, LNLI=20, LNPS=21,
     *   LNPD=22, LMITER=23, LMAXL=24, LKMP=25, LNRMAX=26, LLNWP=27,
     *   LLNIWP=28, LLOCWP=29, LLCIWP=30, LKPRIN=31,
     *   LMXNIT=32, LMXNJ=33, LMXNH=34, LLSOFF=35, LICNS=41)
C
C     Set pointers into RWORK.
C
      PARAMETER (LTSTOP=1, LHMAX=2, LH=3, LTN=4, LCJ=5, LCJOLD=6,
     *   LHOLD=7, LS=8, LROUND=9, LEPLI=10, LSQRN=11, LRSQRN=12,
     *   LEPCON=13, LSTOL=14, LEPIN=15,
     *   LALPHA=21, LBETA=27, LGAMMA=33, LPSI=39, LSIGMA=45, LDELTA=51)
C
      SAVE LID, LENID, NONNEG
C
C
C***FIRST EXECUTABLE STATEMENT  DDASPK
C
C
      IF(INFO(1).NE.0) GO TO 100
C
C-----------------------------------------------------------------------
C     This block is executed for the initial call only.
C     It contains checking of inputs and initializations.
C-----------------------------------------------------------------------
C
C     First check INFO array to make sure all elements of INFO
C     Are within the proper range.  (INFO(1) is checked later, because
C     it must be tested on every call.) ITEMP holds the location
C     within INFO which may be out of range.
C
      DO 10 I=2,9
         ITEMP = I
         IF (INFO(I) .NE. 0 .AND. INFO(I) .NE. 1) GO TO 701
 10      CONTINUE
      ITEMP = 10
      IF(INFO(10).LT.0 .OR. INFO(10).GT.3) GO TO 701
      ITEMP = 11
      IF(INFO(11).LT.0 .OR. INFO(11).GT.2) GO TO 701
      DO 15 I=12,17
         ITEMP = I
         IF (INFO(I) .NE. 0 .AND. INFO(I) .NE. 1) GO TO 701
 15      CONTINUE
      ITEMP = 18
      IF(INFO(18).LT.0 .OR. INFO(18).GT.2) GO TO 701

C
C     Check NEQ to see if it is positive.
C
      IF (NEQ .LE. 0) GO TO 702
C
C     Check and compute maximum order.
C
      MXORD=5
      IF (INFO(9) .NE. 0) THEN
         MXORD=IWORK(LMXORD)
         IF (MXORD .LT. 1 .OR. MXORD .GT. 5) GO TO 703
         ENDIF
      IWORK(LMXORD)=MXORD
C
C     Set and/or check inputs for constraint checking (INFO(10) .NE. 0).
C     Set values for ICNFLG, NONNEG, and pointer LID.
C
      ICNFLG = 0
      NONNEG = 0
      LID = LICNS
      IF (INFO(10) .EQ. 0) GO TO 20
      IF (INFO(10) .EQ. 1) THEN
         ICNFLG = 1
         NONNEG = 0
         LID = LICNS + NEQ
      ELSEIF (INFO(10) .EQ. 2) THEN
         ICNFLG = 0
         NONNEG = 1
      ELSE
         ICNFLG = 1
         NONNEG = 1
         LID = LICNS + NEQ
      ENDIF
C
 20   CONTINUE
C
C     Set and/or check inputs for Krylov solver (INFO(12) .NE. 0).
C     If indicated, set default values for MAXL, KMP, NRMAX, and EPLI.
C     Otherwise, verify inputs required for iterative solver.
C
      IF (INFO(12) .EQ. 0) GO TO 25
C
      IWORK(LMITER) = INFO(12)
      IF (INFO(13) .EQ. 0) THEN
         IWORK(LMAXL) = MIN(5,NEQ)
         IWORK(LKMP) = IWORK(LMAXL)
         IWORK(LNRMAX) = 5
         RWORK(LEPLI) = 0.05D0
      ELSE
         IF(IWORK(LMAXL) .LT. 1 .OR. IWORK(LMAXL) .GT. NEQ) GO TO 720
         IF(IWORK(LKMP) .LT. 1 .OR. IWORK(LKMP) .GT. IWORK(LMAXL))
     1      GO TO 721
         IF(IWORK(LNRMAX) .LT. 0) GO TO 722
         IF(RWORK(LEPLI).LE.0.0D0 .OR. RWORK(LEPLI).GE.1.0D0)GO TO 723
         ENDIF
C
 25   CONTINUE
C
C     Set and/or check controls for the initial condition calculation
C     (INFO(11) .GT. 0).  If indicated, set default values.
C     Otherwise, verify inputs required for iterative solver.
C
      IF (INFO(11) .EQ. 0) GO TO 30
      IF (INFO(17) .EQ. 0) THEN
        IWORK(LMXNIT) = 5
        IF (INFO(12) .GT. 0) IWORK(LMXNIT) = 15
        IWORK(LMXNJ) = 6
        IF (INFO(12) .GT. 0) IWORK(LMXNJ) = 2
        IWORK(LMXNH) = 5
        IWORK(LLSOFF) = 0
        RWORK(LEPIN) = 0.01D0
      ELSE
        IF (IWORK(LMXNIT) .LE. 0) GO TO 725
        IF (IWORK(LMXNJ) .LE. 0) GO TO 725
        IF (IWORK(LMXNH) .LE. 0) GO TO 725
        LSOFF = IWORK(LLSOFF)
        IF (LSOFF .LT. 0 .OR. LSOFF .GT. 1) GO TO 725
        IF (RWORK(LEPIN) .LE. 0.0D0) GO TO 725
        ENDIF
C
 30   CONTINUE
C
C     Below is the computation and checking of the work array lengths
C     LENIW and LENRW, using direct methods (INFO(12) = 0) or
C     the Krylov methods (INFO(12) = 1).
C
      LENIC = 0
      IF (INFO(10) .EQ. 1 .OR. INFO(10) .EQ. 3) LENIC = NEQ
      LENID = 0
      IF (INFO(11) .EQ. 1 .OR. INFO(16) .EQ. 1) LENID = NEQ
      IF (INFO(12) .EQ. 0) THEN
C
C        Compute MTYPE, etc.  Check ML and MU.
C
         NCPHI = MAX(MXORD + 1, 4)
         IF(INFO(6).EQ.0) THEN
            LENPD = NEQ**2
            LENRW = 50 + (NCPHI+3)*NEQ + LENPD
            IF(INFO(5).EQ.0) THEN
               IWORK(LMTYPE)=2
            ELSE
               IWORK(LMTYPE)=1
            ENDIF
         ELSE
            IF(IWORK(LML).LT.0.OR.IWORK(LML).GE.NEQ)GO TO 717
            IF(IWORK(LMU).LT.0.OR.IWORK(LMU).GE.NEQ)GO TO 718
            LENPD=(2*IWORK(LML)+IWORK(LMU)+1)*NEQ
            IF(INFO(5).EQ.0) THEN
               IWORK(LMTYPE)=5
               MBAND=IWORK(LML)+IWORK(LMU)+1
               MSAVE=(NEQ/MBAND)+1
               LENRW = 50 + (NCPHI+3)*NEQ + LENPD + 2*MSAVE
            ELSE
               IWORK(LMTYPE)=4
               LENRW = 50 + (NCPHI+3)*NEQ + LENPD
            ENDIF
         ENDIF
C
C        Compute LENIW, LENWP, LENIWP.
C
         LENIW = 40 + LENIC + LENID + NEQ
         LENWP = 0
         LENIWP = 0
C
      ELSE IF (INFO(12) .EQ. 1)  THEN
         MAXL = IWORK(LMAXL)
         LENWP = IWORK(LLNWP)
         LENIWP = IWORK(LLNIWP)
         LENPD = (MAXL+3+MIN0(1,MAXL-IWORK(LKMP)))*NEQ
     1         + (MAXL+3)*MAXL + 1 + LENWP
         LENRW = 50 + (IWORK(LMXORD)+5)*NEQ + LENPD
         LENIW = 40 + LENIC + LENID + LENIWP
C
      ENDIF
      IF(INFO(16) .NE. 0) LENRW = LENRW + NEQ
C
C     Check lengths of RWORK and IWORK.
C
      IWORK(LNIW)=LENIW
      IWORK(LNRW)=LENRW
      IWORK(LNPD)=LENPD
      IWORK(LLOCWP) = LENPD-LENWP+1
      IF(LRW.LT.LENRW)GO TO 704
      IF(LIW.LT.LENIW)GO TO 705
C
C     Check ICNSTR for legality.
C
      IF (LENIC .GT. 0) THEN
        DO 40 I = 1,NEQ
          ICI = IWORK(LICNS-1+I)
          IF (ICI .LT. -2 .OR. ICI .GT. 2) GO TO 726
 40       CONTINUE
        ENDIF
C
C     Check Y for consistency with constraints.
C
      IF (LENIC .GT. 0) THEN
        CALL DCNST0(NEQ,Y,IWORK(LICNS),IRET)
        IF (IRET .NE. 0) GO TO 727
        ENDIF
C
C     Check ID for legality.
C
      IF (LENID .GT. 0) THEN
        DO 50 I = 1,NEQ
          IDI = IWORK(LID-1+I)
          IF (IDI .NE. 1 .AND. IDI .NE. -1) GO TO 724
 50       CONTINUE
        ENDIF
C
C     Check to see that TOUT is different from T.
C
      IF(TOUT .EQ. T)GO TO 719
C
C     Check HMAX.
C
      IF(INFO(7) .NE. 0) THEN
         HMAX = RWORK(LHMAX)
         IF (HMAX .LE. 0.0D0) GO TO 710
         ENDIF
C
C     Initialize counters and other flags.
C
      IWORK(LNST)=0
      IWORK(LNRE)=0
      IWORK(LNJE)=0
      IWORK(LETF)=0
      IWORK(LNCFN)=0
      IWORK(LNNI)=0
      IWORK(LNLI)=0
      IWORK(LNPS)=0
      IWORK(LNCFL)=0
      IWORK(LKPRIN)=INFO(18)
      IDID=1
      GO TO 200
C
C-----------------------------------------------------------------------
C     This block is for continuation calls only.
C     Here we check INFO(1), and if the last step was interrupted,
C     we check whether appropriate action was taken.
C-----------------------------------------------------------------------
C
100   CONTINUE
      IF(INFO(1).EQ.1)GO TO 110
      ITEMP = 1
      IF(INFO(1).NE.-1)GO TO 701
C
C     If we are here, the last step was interrupted by an error
C     condition from DDSTP, and appropriate action was not taken.
C     This is a fatal error.
C
      MSG = 'DASPK--  THE LAST STEP TERMINATED WITH A NEGATIVE'
      CALL XERRWD(MSG,49,201,0,0,0,0,0,0.0D0,0.0D0)
      MSG = 'DASPK--  VALUE (=I1) OF IDID AND NO APPROPRIATE'
      CALL XERRWD(MSG,47,202,0,1,IDID,0,0,0.0D0,0.0D0)
      MSG = 'DASPK--  ACTION WAS TAKEN. RUN TERMINATED'
      CALL XERRWD(MSG,41,203,1,0,0,0,0,0.0D0,0.0D0)
      RETURN
110   CONTINUE
C
C-----------------------------------------------------------------------
C     This block is executed on all calls.
C
C     Counters are saved for later checks of performance.
C     Then the error tolerance parameters are checked, and the
C     work array pointers are set.
C-----------------------------------------------------------------------
C
200   CONTINUE
C
C     Save counters for use later.
C
      IWORK(LNSTL)=IWORK(LNST)
      NLI0 = IWORK(LNLI)
      NNI0 = IWORK(LNNI)
      NCFN0 = IWORK(LNCFN)
      NCFL0 = IWORK(LNCFL)
      NWARN = 0
C
C     Check RTOL and ATOL.
C
      NZFLG = 0
      RTOLI = RTOL(1)
      ATOLI = ATOL(1)
      DO 210 I=1,NEQ
         IF (INFO(2) .EQ. 1) RTOLI = RTOL(I)
         IF (INFO(2) .EQ. 1) ATOLI = ATOL(I)
         IF (RTOLI .GT. 0.0D0 .OR. ATOLI .GT. 0.0D0) NZFLG = 1
         IF (RTOLI .LT. 0.0D0) GO TO 706
         IF (ATOLI .LT. 0.0D0) GO TO 707
210      CONTINUE
      IF (NZFLG .EQ. 0) GO TO 708
C
C     Set pointers to RWORK and IWORK segments.
C     For direct methods, SAVR is not used.
C
      IWORK(LLCIWP) = LID + LENID
      LSAVR = LDELTA
      IF (INFO(12) .NE. 0) LSAVR = LDELTA + NEQ
      LE = LSAVR + NEQ
      LWT = LE + NEQ
      LVT = LWT
      IF (INFO(16) .NE. 0) LVT = LWT + NEQ
      LPHI = LVT + NEQ
      LWM = LPHI + (IWORK(LMXORD)+1)*NEQ
      IF (INFO(1) .EQ. 1) GO TO 400
C
C-----------------------------------------------------------------------
C     This block is executed on the initial call only.
C     Set the initial step size, the error weight vector, and PHI.
C     Compute unknown initial components of Y and YPRIME, if requested.
C-----------------------------------------------------------------------
C
300   CONTINUE
      TN=T
      IDID=1
C
C     Set error weight array WT and altered weight array VT.
C
      CALL DDAWTS(NEQ,INFO(2),RTOL,ATOL,Y,RWORK(LWT),RPAR,IPAR)
      CALL DINVWT(NEQ,RWORK(LWT),IER)
      IF (IER .NE. 0) GO TO 713
      IF (INFO(16) .NE. 0) THEN
        DO 305 I = 1, NEQ
 305      RWORK(LVT+I-1) = MAX(IWORK(LID+I-1),0)*RWORK(LWT+I-1)
        ENDIF
C
C     Compute unit roundoff and HMIN.
C
      UROUND = D1MACH(4)
      RWORK(LROUND) = UROUND
      HMIN = 4.0D0*UROUND*MAX(ABS(T),ABS(TOUT))
C
C     Set/check STPTOL control for initial condition calculation.
C
      IF (INFO(11) .NE. 0) THEN
        IF( INFO(17) .EQ. 0) THEN
          RWORK(LSTOL) = UROUND**.6667D0
        ELSE
          IF (RWORK(LSTOL) .LE. 0.0D0) GO TO 725
          ENDIF
        ENDIF
C
C     Compute EPCON and square root of NEQ and its reciprocal, used
C     inside iterative solver.
C
      RWORK(LEPCON) = 0.33D0
      FLOATN = NEQ
      RWORK(LSQRN) = SQRT(FLOATN)
      RWORK(LRSQRN) = 1.D0/RWORK(LSQRN)
C
C     Check initial interval to see that it is long enough.
C
      TDIST = ABS(TOUT - T)
      IF(TDIST .LT. HMIN) GO TO 714
C
C     Check H0, if this was input.
C
      IF (INFO(8) .EQ. 0) GO TO 310
         H0 = RWORK(LH)
         IF ((TOUT - T)*H0 .LT. 0.0D0) GO TO 711
         IF (H0 .EQ. 0.0D0) GO TO 712
         GO TO 320
310    CONTINUE
C
C     Compute initial stepsize, to be used by either
C     DDSTP or DDASIC, depending on INFO(11).
C
      H0 = 0.001D0*TDIST
      YPNORM = DDWNRM(NEQ,YPRIME,RWORK(LVT),RPAR,IPAR)
      IF (YPNORM .GT. 0.5D0/H0) H0 = 0.5D0/YPNORM
      H0 = SIGN(H0,TOUT-T)
C
C     Adjust H0 if necessary to meet HMAX bound.
C
320   IF (INFO(7) .EQ. 0) GO TO 330
         RH = ABS(H0)/RWORK(LHMAX)
         IF (RH .GT. 1.0D0) H0 = H0/RH
C
C     Check against TSTOP, if applicable.
C
330   IF (INFO(4) .EQ. 0) GO TO 340
         TSTOP = RWORK(LTSTOP)
         IF ((TSTOP - T)*H0 .LT. 0.0D0) GO TO 715
         IF ((T + H0 - TSTOP)*H0 .GT. 0.0D0) H0 = TSTOP - T
         IF ((TSTOP - TOUT)*H0 .LT. 0.0D0) GO TO 709
C
340   IF (INFO(11) .EQ. 0) GO TO 370
C
C     Compute unknown components of initial Y and YPRIME, depending
C     on INFO(11) and INFO(12).  INFO(12) represents the nonlinear
C     solver type (direct/Krylov).  Pass the name of the specific
C     nonlinear solver, depending on INFO(12).  The location of the work
C     arrays SAVR, YIC, YPIC, PWK also differ in the two cases.
C
      NWT = 1
      EPCONI = RWORK(LEPIN)*RWORK(LEPCON)
350   IF (INFO(12) .EQ. 0) THEN
         LYIC = LPHI + 2*NEQ
         LYPIC = LYIC + NEQ
         LPWK = LYPIC
         CALL DDASIC(TN,Y,YPRIME,NEQ,INFO(11),IWORK(LID),
     *     RES,JAC,PSOL,H0,RWORK(LWT),NWT,IDID,RPAR,IPAR,
     *     RWORK(LPHI),RWORK(LSAVR),RWORK(LDELTA),RWORK(LE),
     *     RWORK(LYIC),RWORK(LYPIC),RWORK(LPWK),RWORK(LWM),IWORK(LIWM),
     *     HMIN,RWORK(LROUND),RWORK(LEPLI),RWORK(LSQRN),RWORK(LRSQRN),
     *     EPCONI,RWORK(LSTOL),INFO(15),ICNFLG,IWORK(LICNS),DDASID)
      ELSE IF (INFO(12) .EQ. 1) THEN
         LYIC = LWM
         LYPIC = LYIC + NEQ
         LPWK = LYPIC + NEQ
         CALL DDASIC(TN,Y,YPRIME,NEQ,INFO(11),IWORK(LID),
     *     RES,JAC,PSOL,H0,RWORK(LWT),NWT,IDID,RPAR,IPAR,
     *     RWORK(LPHI),RWORK(LSAVR),RWORK(LDELTA),RWORK(LE),
     *     RWORK(LYIC),RWORK(LYPIC),RWORK(LPWK),RWORK(LWM),IWORK(LIWM),
     *     HMIN,RWORK(LROUND),RWORK(LEPLI),RWORK(LSQRN),RWORK(LRSQRN),
     *     EPCONI,RWORK(LSTOL),INFO(15),ICNFLG,IWORK(LICNS),DDASIK)
      ENDIF
C
      IF (IDID .LT. 0) GO TO 600
C
C     DDASIC was successful.  If this was the first call to DDASIC,
C     update the WT array (with the current Y) and call it again.
C
      IF (NWT .EQ. 2) GO TO 355
      NWT = 2
      CALL DDAWTS(NEQ,INFO(2),RTOL,ATOL,Y,RWORK(LWT),RPAR,IPAR)
      CALL DINVWT(NEQ,RWORK(LWT),IER)
      IF (IER .NE. 0) GO TO 713
      GO TO 350
C
C     If INFO(14) = 1, return now with IDID = 4.
C
355   IF (INFO(14) .EQ. 1) THEN
        IDID = 4
        H = H0
        IF (INFO(11) .EQ. 1) RWORK(LHOLD) = H0
        GO TO 590
      ENDIF
C
C     Update the WT and VT arrays one more time, with the new Y.
C
      CALL DDAWTS(NEQ,INFO(2),RTOL,ATOL,Y,RWORK(LWT),RPAR,IPAR)
      CALL DINVWT(NEQ,RWORK(LWT),IER)
      IF (IER .NE. 0) GO TO 713
      IF (INFO(16) .NE. 0) THEN
        DO 357 I = 1, NEQ
 357      RWORK(LVT+I-1) = MAX(IWORK(LID+I-1),0)*RWORK(LWT+I-1)
        ENDIF
C
C     Reset the initial stepsize to be used by DDSTP.
C     Use H0, if this was input.  Otherwise, recompute H0,
C     and adjust it if necessary to meet HMAX bound.
C
      IF (INFO(8) .NE. 0) THEN
         H0 = RWORK(LH)
         GO TO 360
         ENDIF
C
      H0 = 0.001D0*TDIST
      YPNORM = DDWNRM(NEQ,YPRIME,RWORK(LVT),RPAR,IPAR)
      IF (YPNORM .GT. 0.5D0/H0) H0 = 0.5D0/YPNORM
      H0 = SIGN(H0,TOUT-T)
C
360   IF (INFO(7) .NE. 0) THEN
         RH = ABS(H0)/RWORK(LHMAX)
         IF (RH .GT. 1.0D0) H0 = H0/RH
         ENDIF
C
C     Check against TSTOP, if applicable.
C
      IF (INFO(4) .NE. 0) THEN
         TSTOP = RWORK(LTSTOP)
         IF ((T + H0 - TSTOP)*H0 .GT. 0.0D0) H0 = TSTOP - T
         ENDIF
C
C     Load H and RWORK(LH) with H0.
C
370   H = H0
      RWORK(LH) = H
C
C     Load Y and H*YPRIME into PHI(*,1) and PHI(*,2).
C
      ITEMP = LPHI + NEQ
      DO 380 I = 1,NEQ
         RWORK(LPHI + I - 1) = Y(I)
380      RWORK(ITEMP + I - 1) = H*YPRIME(I)
C
      GO TO 500
C
C-----------------------------------------------------------------------
C     This block is for continuation calls only.
C     Its purpose is to check stop conditions before taking a step.
C     Adjust H if necessary to meet HMAX bound.
C-----------------------------------------------------------------------
C
400   CONTINUE
      UROUND=RWORK(LROUND)
      DONE = .FALSE.
      TN=RWORK(LTN)
      H=RWORK(LH)
      IF(INFO(7) .EQ. 0) GO TO 410
         RH = ABS(H)/RWORK(LHMAX)
         IF(RH .GT. 1.0D0) H = H/RH
410   CONTINUE
      IF(T .EQ. TOUT) GO TO 719
      IF((T - TOUT)*H .GT. 0.0D0) GO TO 711
      IF(INFO(4) .EQ. 1) GO TO 430
      IF(INFO(3) .EQ. 1) GO TO 420
      IF((TN-TOUT)*H.LT.0.0D0)GO TO 490
      CALL DDATRP(TN,TOUT,Y,YPRIME,NEQ,IWORK(LKOLD),
     *  RWORK(LPHI),RWORK(LPSI))
      T=TOUT
      IDID = 3
      DONE = .TRUE.
      GO TO 490
420   IF((TN-T)*H .LE. 0.0D0) GO TO 490
      IF((TN - TOUT)*H .GT. 0.0D0) GO TO 425
      CALL DDATRP(TN,TN,Y,YPRIME,NEQ,IWORK(LKOLD),
     *  RWORK(LPHI),RWORK(LPSI))
      T = TN
      IDID = 1
      DONE = .TRUE.
      GO TO 490
425   CONTINUE
      CALL DDATRP(TN,TOUT,Y,YPRIME,NEQ,IWORK(LKOLD),
     *  RWORK(LPHI),RWORK(LPSI))
      T = TOUT
      IDID = 3
      DONE = .TRUE.
      GO TO 490
430   IF(INFO(3) .EQ. 1) GO TO 440
      TSTOP=RWORK(LTSTOP)
      IF((TN-TSTOP)*H.GT.0.0D0) GO TO 715
      IF((TSTOP-TOUT)*H.LT.0.0D0)GO TO 709
      IF((TN-TOUT)*H.LT.0.0D0)GO TO 450
      CALL DDATRP(TN,TOUT,Y,YPRIME,NEQ,IWORK(LKOLD),
     *   RWORK(LPHI),RWORK(LPSI))
      T=TOUT
      IDID = 3
      DONE = .TRUE.
      GO TO 490
440   TSTOP = RWORK(LTSTOP)
      IF((TN-TSTOP)*H .GT. 0.0D0) GO TO 715
      IF((TSTOP-TOUT)*H .LT. 0.0D0) GO TO 709
      IF((TN-T)*H .LE. 0.0D0) GO TO 450
      IF((TN - TOUT)*H .GT. 0.0D0) GO TO 445
      CALL DDATRP(TN,TN,Y,YPRIME,NEQ,IWORK(LKOLD),
     *  RWORK(LPHI),RWORK(LPSI))
      T = TN
      IDID = 1
      DONE = .TRUE.
      GO TO 490
445   CONTINUE
      CALL DDATRP(TN,TOUT,Y,YPRIME,NEQ,IWORK(LKOLD),
     *  RWORK(LPHI),RWORK(LPSI))
      T = TOUT
      IDID = 3
      DONE = .TRUE.
      GO TO 490
450   CONTINUE
C
C     Check whether we are within roundoff of TSTOP.
C
      IF(ABS(TN-TSTOP).GT.100.0D0*UROUND*
     *   (ABS(TN)+ABS(H)))GO TO 460
      CALL DDATRP(TN,TSTOP,Y,YPRIME,NEQ,IWORK(LKOLD),
     *  RWORK(LPHI),RWORK(LPSI))
      IDID=2
      T=TSTOP
      DONE = .TRUE.
      GO TO 490
460   TNEXT=TN+H
      IF((TNEXT-TSTOP)*H.LE.0.0D0)GO TO 490
      H=TSTOP-TN
      RWORK(LH)=H
C
490   IF (DONE) GO TO 590
C
C-----------------------------------------------------------------------
C     The next block contains the call to the one-step integrator DDSTP.
C     This is a looping point for the integration steps.
C     Check for too many steps.
C     Check for poor Newton/Krylov performance.
C     Update WT.  Check for too much accuracy requested.
C     Compute minimum stepsize.
C-----------------------------------------------------------------------
C
500   CONTINUE
C
C     Check for too many steps.
C
      IF((IWORK(LNST)-IWORK(LNSTL)).LT.500) GO TO 505
           IDID=-1
           GO TO 527
C
C Check for poor Newton/Krylov performance.
C
505   IF (INFO(12) .EQ. 0) GO TO 510
      NSTD = IWORK(LNST) - IWORK(LNSTL)
      NNID = IWORK(LNNI) - NNI0
      IF (NSTD .LT. 10 .OR. NNID .EQ. 0) GO TO 510
      AVLIN = REAL(IWORK(LNLI) - NLI0)/REAL(NNID)
      RCFN = REAL(IWORK(LNCFN) - NCFN0)/REAL(NSTD)
      RCFL = REAL(IWORK(LNCFL) - NCFL0)/REAL(NNID)
      FMAXL = IWORK(LMAXL)
      LAVL = AVLIN .GT. FMAXL
      LCFN = RCFN .GT. 0.9D0
      LCFL = RCFL .GT. 0.9D0
      LWARN = LAVL .OR. LCFN .OR. LCFL
      IF (.NOT.LWARN) GO TO 510
      NWARN = NWARN + 1
      IF (NWARN .GT. 10) GO TO 510
      IF (LAVL) THEN
        MSG = 'DASPK-- Warning. Poor iterative algorithm performance   '
        CALL XERRWD (MSG, 56, 501, 0, 0, 0, 0, 0, 0.0D0, 0.0D0)
        MSG = '      at T = R1. Average no. of linear iterations = R2  '
        CALL XERRWD (MSG, 56, 501, 0, 0, 0, 0, 2, TN, AVLIN)
        ENDIF
      IF (LCFN) THEN
        MSG = 'DASPK-- Warning. Poor iterative algorithm performance   '
        CALL XERRWD (MSG, 56, 502, 0, 0, 0, 0, 0, 0.0D0, 0.0D0)
        MSG = '      at T = R1. Nonlinear convergence failure rate = R2'
        CALL XERRWD (MSG, 56, 502, 0, 0, 0, 0, 2, TN, RCFN)
        ENDIF
      IF (LCFL) THEN
        MSG = 'DASPK-- Warning. Poor iterative algorithm performance   '
        CALL XERRWD (MSG, 56, 503, 0, 0, 0, 0, 0, 0.0D0, 0.0D0)
        MSG = '      at T = R1. Linear convergence failure rate = R2   '
        CALL XERRWD (MSG, 56, 503, 0, 0, 0, 0, 2, TN, RCFL)
        ENDIF
C
C     Update WT and VT, if this is not the first call.
C
510   CALL DDAWTS(NEQ,INFO(2),RTOL,ATOL,RWORK(LPHI),RWORK(LWT),
     *            RPAR,IPAR)
      CALL DINVWT(NEQ,RWORK(LWT),IER)
      IF (IER .NE. 0) THEN
        IDID = -3
        GO TO 527
        ENDIF
      IF (INFO(16) .NE. 0) THEN
        DO 515 I = 1, NEQ
 515      RWORK(LVT+I-1) = MAX(IWORK(LID+I-1),0)*RWORK(LWT+I-1)
        ENDIF
C
C     Test for too much accuracy requested.
C
      R = DDWNRM(NEQ,RWORK(LPHI),RWORK(LWT),RPAR,IPAR)*100.0D0*UROUND
      IF (R .LE. 1.0D0) GO TO 525
C
C     Multiply RTOL and ATOL by R and return.
C
      IF(INFO(2).EQ.1)GO TO 523
           RTOL(1)=R*RTOL(1)
           ATOL(1)=R*ATOL(1)
           IDID=-2
           GO TO 527
523   DO 524 I=1,NEQ
           RTOL(I)=R*RTOL(I)
524        ATOL(I)=R*ATOL(I)
      IDID=-2
      GO TO 527
525   CONTINUE
C
C     Compute minimum stepsize.
C
      HMIN=4.0D0*UROUND*MAX(ABS(TN),ABS(TOUT))
C
C     Test H vs. HMAX
      IF (INFO(7) .NE. 0) THEN
         RH = ABS(H)/RWORK(LHMAX)
         IF (RH .GT. 1.0D0) H = H/RH
         ENDIF
C
C     Call the one-step integrator.
C     Note that INFO(12) represents the nonlinear solver type.
C     Pass the required nonlinear solver, depending upon INFO(12).
C
      IF (INFO(12) .EQ. 0) THEN
         CALL DDSTP(TN,Y,YPRIME,NEQ,
     *      RES,JAC,PSOL,H,RWORK(LWT),RWORK(LVT),INFO(1),IDID,RPAR,IPAR,
     *      RWORK(LPHI),RWORK(LSAVR),RWORK(LDELTA),RWORK(LE),
     *      RWORK(LWM),IWORK(LIWM),
     *      RWORK(LALPHA),RWORK(LBETA),RWORK(LGAMMA),
     *      RWORK(LPSI),RWORK(LSIGMA),
     *      RWORK(LCJ),RWORK(LCJOLD),RWORK(LHOLD),RWORK(LS),HMIN,
     *      RWORK(LROUND), RWORK(LEPLI),RWORK(LSQRN),RWORK(LRSQRN),
     *      RWORK(LEPCON), IWORK(LPHASE),IWORK(LJCALC),INFO(15),
     *      IWORK(LK), IWORK(LKOLD),IWORK(LNS),NONNEG,INFO(12),
     *      DNEDD)
      ELSE IF (INFO(12) .EQ. 1) THEN
         CALL DDSTP(TN,Y,YPRIME,NEQ,
     *      RES,JAC,PSOL,H,RWORK(LWT),RWORK(LVT),INFO(1),IDID,RPAR,IPAR,
     *      RWORK(LPHI),RWORK(LSAVR),RWORK(LDELTA),RWORK(LE),
     *      RWORK(LWM),IWORK(LIWM),
     *      RWORK(LALPHA),RWORK(LBETA),RWORK(LGAMMA),
     *      RWORK(LPSI),RWORK(LSIGMA),
     *      RWORK(LCJ),RWORK(LCJOLD),RWORK(LHOLD),RWORK(LS),HMIN,
     *      RWORK(LROUND), RWORK(LEPLI),RWORK(LSQRN),RWORK(LRSQRN),
     *      RWORK(LEPCON), IWORK(LPHASE),IWORK(LJCALC),INFO(15),
     *      IWORK(LK), IWORK(LKOLD),IWORK(LNS),NONNEG,INFO(12),
     *      DNEDK)
      ENDIF
C
527   IF(IDID.LT.0)GO TO 600
C
C-----------------------------------------------------------------------
C     This block handles the case of a successful return from DDSTP
C     (IDID=1).  Test for stop conditions.
C-----------------------------------------------------------------------
C
      IF(INFO(4).NE.0)GO TO 540
           IF(INFO(3).NE.0)GO TO 530
             IF((TN-TOUT)*H.LT.0.0D0)GO TO 500
             CALL DDATRP(TN,TOUT,Y,YPRIME,NEQ,
     *         IWORK(LKOLD),RWORK(LPHI),RWORK(LPSI))
             IDID=3
             T=TOUT
             GO TO 580
530          IF((TN-TOUT)*H.GE.0.0D0)GO TO 535
             T=TN
             IDID=1
             GO TO 580
535          CALL DDATRP(TN,TOUT,Y,YPRIME,NEQ,
     *         IWORK(LKOLD),RWORK(LPHI),RWORK(LPSI))
             IDID=3
             T=TOUT
             GO TO 580
540   IF(INFO(3).NE.0)GO TO 550
      IF((TN-TOUT)*H.LT.0.0D0)GO TO 542
         CALL DDATRP(TN,TOUT,Y,YPRIME,NEQ,
     *     IWORK(LKOLD),RWORK(LPHI),RWORK(LPSI))
         T=TOUT
         IDID=3
         GO TO 580
542   IF(ABS(TN-TSTOP).LE.100.0D0*UROUND*
     *   (ABS(TN)+ABS(H)))GO TO 545
      TNEXT=TN+H
      IF((TNEXT-TSTOP)*H.LE.0.0D0)GO TO 500
      H=TSTOP-TN
      GO TO 500
545   CALL DDATRP(TN,TSTOP,Y,YPRIME,NEQ,
     *  IWORK(LKOLD),RWORK(LPHI),RWORK(LPSI))
      IDID=2
      T=TSTOP
      GO TO 580
550   IF((TN-TOUT)*H.GE.0.0D0)GO TO 555
      IF(ABS(TN-TSTOP).LE.100.0D0*UROUND*(ABS(TN)+ABS(H)))GO TO 552
      T=TN
      IDID=1
      GO TO 580
552   CALL DDATRP(TN,TSTOP,Y,YPRIME,NEQ,
     *  IWORK(LKOLD),RWORK(LPHI),RWORK(LPSI))
      IDID=2
      T=TSTOP
      GO TO 580
555   CALL DDATRP(TN,TOUT,Y,YPRIME,NEQ,
     *   IWORK(LKOLD),RWORK(LPHI),RWORK(LPSI))
      T=TOUT
      IDID=3
580   CONTINUE
C
C-----------------------------------------------------------------------
C     All successful returns from DDASPK are made from this block.
C-----------------------------------------------------------------------
C
590   CONTINUE
      RWORK(LTN)=TN
      RWORK(LH)=H
      RETURN
C
C-----------------------------------------------------------------------
C     This block handles all unsuccessful returns other than for
C     illegal input.
C-----------------------------------------------------------------------
C
600   CONTINUE
      ITEMP = -IDID
      GO TO (610,620,630,700,655,640,650,660,670,675,
     *  680,685,690,695), ITEMP
C
C     The maximum number of steps was taken before
C     reaching tout.
C
610   MSG = 'DASPK--  AT CURRENT T (=R1)  500 STEPS'
      CALL XERRWD(MSG,38,610,0,0,0,0,1,TN,0.0D0)
      MSG = 'DASPK--  TAKEN ON THIS CALL BEFORE REACHING TOUT'
      CALL XERRWD(MSG,48,611,0,0,0,0,0,0.0D0,0.0D0)
      GO TO 700
C
C     Too much accuracy for machine precision.
C
620   MSG = 'DASPK--  AT T (=R1) TOO MUCH ACCURACY REQUESTED'
      CALL XERRWD(MSG,47,620,0,0,0,0,1,TN,0.0D0)
      MSG = 'DASPK--  FOR PRECISION OF MACHINE. RTOL AND ATOL'
      CALL XERRWD(MSG,48,621,0,0,0,0,0,0.0D0,0.0D0)
      MSG = 'DASPK--  WERE INCREASED TO APPROPRIATE VALUES'
      CALL XERRWD(MSG,45,622,0,0,0,0,0,0.0D0,0.0D0)
      GO TO 700
C
C     WT(I) .LE. 0.0D0 for some I (not at start of problem).
C
630   MSG = 'DASPK--  AT T (=R1) SOME ELEMENT OF WT'
      CALL XERRWD(MSG,38,630,0,0,0,0,1,TN,0.0D0)
      MSG = 'DASPK--  HAS BECOME .LE. 0.0'
      CALL XERRWD(MSG,28,631,0,0,0,0,0,0.0D0,0.0D0)
      GO TO 700
C
C     Error test failed repeatedly or with H=HMIN.
C
640   MSG = 'DASPK--  AT T (=R1) AND STEPSIZE H (=R2) THE'
      CALL XERRWD(MSG,44,640,0,0,0,0,2,TN,H)
      MSG='DASPK--  ERROR TEST FAILED REPEATEDLY OR WITH ABS(H)=HMIN'
      CALL XERRWD(MSG,57,641,0,0,0,0,0,0.0D0,0.0D0)
      GO TO 700
C
C     Nonlinear solver failed to converge repeatedly or with H=HMIN.
C
650   MSG = 'DASPK--  AT T (=R1) AND STEPSIZE H (=R2) THE'
      CALL XERRWD(MSG,44,650,0,0,0,0,2,TN,H)
      MSG = 'DASPK--  NONLINEAR SOLVER FAILED TO CONVERGE'
      CALL XERRWD(MSG,44,651,0,0,0,0,0,0.0D0,0.0D0)
      MSG = 'DASPK--  REPEATEDLY OR WITH ABS(H)=HMIN'
      CALL XERRWD(MSG,40,652,0,0,0,0,0,0.0D0,0.0D0)
      GO TO 700
C
C     The preconditioner had repeated failures.
C
655   MSG = 'DASPK--  AT T (=R1) AND STEPSIZE H (=R2) THE'
      CALL XERRWD(MSG,44,655,0,0,0,0,2,TN,H)
      MSG = 'DASPK--  PRECONDITIONER HAD REPEATED FAILURES.'
      CALL XERRWD(MSG,46,656,0,0,0,0,0,0.0D0,0.0D0)
      GO TO 700
C
C     The iteration matrix is singular.
C
660   MSG = 'DASPK--  AT T (=R1) AND STEPSIZE H (=R2) THE'
      CALL XERRWD(MSG,44,660,0,0,0,0,2,TN,H)
      MSG = 'DASPK--  ITERATION MATRIX IS SINGULAR.'
      CALL XERRWD(MSG,38,661,0,0,0,0,0,0.0D0,0.0D0)
      GO TO 700
C
C     Nonlinear system failure preceded by error test failures.
C
670   MSG = 'DASPK--  AT T (=R1) AND STEPSIZE H (=R2) THE'
      CALL XERRWD(MSG,44,670,0,0,0,0,2,TN,H)
      MSG = 'DASPK--  NONLINEAR SOLVER COULD NOT CONVERGE.'
      CALL XERRWD(MSG,45,671,0,0,0,0,0,0.0D0,0.0D0)
      MSG = 'DASPK--  ALSO, THE ERROR TEST FAILED REPEATEDLY.'
      CALL XERRWD(MSG,49,672,0,0,0,0,0,0.0D0,0.0D0)
      GO TO 700
C
C     Nonlinear system failure because IRES = -1.
C
675   MSG = 'DASPK--  AT T (=R1) AND STEPSIZE H (=R2) THE'
      CALL XERRWD(MSG,44,675,0,0,0,0,2,TN,H)
      MSG = 'DASPK--  NONLINEAR SYSTEM SOLVER COULD NOT CONVERGE'
      CALL XERRWD(MSG,51,676,0,0,0,0,0,0.0D0,0.0D0)
      MSG = 'DASPK--  BECAUSE IRES WAS EQUAL TO MINUS ONE'
      CALL XERRWD(MSG,44,677,0,0,0,0,0,0.0D0,0.0D0)
      GO TO 700
C
C     Failure because IRES = -2.
C
680   MSG = 'DASPK--  AT T (=R1) AND STEPSIZE H (=R2)'
      CALL XERRWD(MSG,40,680,0,0,0,0,2,TN,H)
      MSG = 'DASPK--  IRES WAS EQUAL TO MINUS TWO'
      CALL XERRWD(MSG,36,681,0,0,0,0,0,0.0D0,0.0D0)
      GO TO 700
C
C     Failed to compute initial YPRIME.
C
685   MSG = 'DASPK--  AT T (=R1) AND STEPSIZE H (=R2) THE'
      CALL XERRWD(MSG,44,685,0,0,0,0,0,0.0D0,0.0D0)
      MSG = 'DASPK--  INITIAL (Y,YPRIME) COULD NOT BE COMPUTED'
      CALL XERRWD(MSG,49,686,0,0,0,0,2,TN,H0)
      GO TO 700
C
C     Failure because IER was negative from PSOL.
C
690   MSG = 'DASPK--  AT T (=R1) AND STEPSIZE H (=R2)'
      CALL XERRWD(MSG,40,690,0,0,0,0,2,TN,H)
      MSG = 'DASPK--  IER WAS NEGATIVE FROM PSOL'
      CALL XERRWD(MSG,35,691,0,0,0,0,0,0.0D0,0.0D0)
      GO TO 700
C
C     Failure because the linear system solver could not converge.
C
695   MSG = 'DASPK--  AT T (=R1) AND STEPSIZE H (=R2) THE'
      CALL XERRWD(MSG,44,695,0,0,0,0,2,TN,H)
      MSG = 'DASPK--  LINEAR SYSTEM SOLVER COULD NOT CONVERGE.'
      CALL XERRWD(MSG,50,696,0,0,0,0,0,0.0D0,0.0D0)
      GO TO 700
C
C
700   CONTINUE
      INFO(1)=-1
      T=TN
      RWORK(LTN)=TN
      RWORK(LH)=H
      RETURN
C
C-----------------------------------------------------------------------
C     This block handles all error returns due to illegal input,
C     as detected before calling DDSTP.
C     First the error message routine is called.  If this happens
C     twice in succession, execution is terminated.
C-----------------------------------------------------------------------
C
701   MSG = 'DASPK--  ELEMENT (=I1) OF INFO VECTOR IS NOT VALID'
      CALL XERRWD(MSG,50,1,0,1,ITEMP,0,0,0.0D0,0.0D0)
      GO TO 750
702   MSG = 'DASPK--  NEQ (=I1) .LE. 0'
      CALL XERRWD(MSG,25,2,0,1,NEQ,0,0,0.0D0,0.0D0)
      GO TO 750
703   MSG = 'DASPK--  MAXORD (=I1) NOT IN RANGE'
      CALL XERRWD(MSG,34,3,0,1,MXORD,0,0,0.0D0,0.0D0)
      GO TO 750
704   MSG='DASPK--  RWORK LENGTH NEEDED, LENRW (=I1), EXCEEDS LRW (=I2)'
      CALL XERRWD(MSG,60,4,0,2,LENRW,LRW,0,0.0D0,0.0D0)
      GO TO 750
705   MSG='DASPK--  IWORK LENGTH NEEDED, LENIW (=I1), EXCEEDS LIW (=I2)'
      CALL XERRWD(MSG,60,5,0,2,LENIW,LIW,0,0.0D0,0.0D0)
      GO TO 750
706   MSG = 'DASPK--  SOME ELEMENT OF RTOL IS .LT. 0'
      CALL XERRWD(MSG,39,6,0,0,0,0,0,0.0D0,0.0D0)
      GO TO 750
707   MSG = 'DASPK--  SOME ELEMENT OF ATOL IS .LT. 0'
      CALL XERRWD(MSG,39,7,0,0,0,0,0,0.0D0,0.0D0)
      GO TO 750
708   MSG = 'DASPK--  ALL ELEMENTS OF RTOL AND ATOL ARE ZERO'
      CALL XERRWD(MSG,47,8,0,0,0,0,0,0.0D0,0.0D0)
      GO TO 750
709   MSG='DASPK--  INFO(4) = 1 AND TSTOP (=R1) BEHIND TOUT (=R2)'
      CALL XERRWD(MSG,54,9,0,0,0,0,2,TSTOP,TOUT)
      GO TO 750
710   MSG = 'DASPK--  HMAX (=R1) .LT. 0.0'
      CALL XERRWD(MSG,28,10,0,0,0,0,1,HMAX,0.0D0)
      GO TO 750
711   MSG = 'DASPK--  TOUT (=R1) BEHIND T (=R2)'
      CALL XERRWD(MSG,34,11,0,0,0,0,2,TOUT,T)
      GO TO 750
712   MSG = 'DASPK--  INFO(8)=1 AND H0=0.0'
      CALL XERRWD(MSG,29,12,0,0,0,0,0,0.0D0,0.0D0)
      GO TO 750
713   MSG = 'DASPK--  SOME ELEMENT OF WT IS .LE. 0.0'
      CALL XERRWD(MSG,39,13,0,0,0,0,0,0.0D0,0.0D0)
      GO TO 750
714   MSG='DASPK-- TOUT (=R1) TOO CLOSE TO T (=R2) TO START INTEGRATION'
      CALL XERRWD(MSG,60,14,0,0,0,0,2,TOUT,T)
      GO TO 750
715   MSG = 'DASPK--  INFO(4)=1 AND TSTOP (=R1) BEHIND T (=R2)'
      CALL XERRWD(MSG,49,15,0,0,0,0,2,TSTOP,T)
      GO TO 750
717   MSG = 'DASPK--  ML (=I1) ILLEGAL. EITHER .LT. 0 OR .GT. NEQ'
      CALL XERRWD(MSG,52,17,0,1,IWORK(LML),0,0,0.0D0,0.0D0)
      GO TO 750
718   MSG = 'DASPK--  MU (=I1) ILLEGAL. EITHER .LT. 0 OR .GT. NEQ'
      CALL XERRWD(MSG,52,18,0,1,IWORK(LMU),0,0,0.0D0,0.0D0)
      GO TO 750
719   MSG = 'DASPK--  TOUT (=R1) IS EQUAL TO T (=R2)'
      CALL XERRWD(MSG,39,19,0,0,0,0,2,TOUT,T)
      GO TO 750
720   MSG = 'DASPK--  MAXL (=I1) ILLEGAL. EITHER .LT. 1 OR .GT. NEQ'
      CALL XERRWD(MSG,54,20,0,1,IWORK(LMAXL),0,0,0.0D0,0.0D0)
      GO TO 750
721   MSG = 'DASPK--  KMP (=I1) ILLEGAL. EITHER .LT. 1 OR .GT. MAXL'
      CALL XERRWD(MSG,54,21,0,1,IWORK(LKMP),0,0,0.0D0,0.0D0)
      GO TO 750
722   MSG = 'DASPK--  NRMAX (=I1) ILLEGAL. .LT. 0'
      CALL XERRWD(MSG,36,22,0,1,IWORK(LNRMAX),0,0,0.0D0,0.0D0)
      GO TO 750
723   MSG = 'DASPK--  EPLI (=R1) ILLEGAL. EITHER .LE. 0.D0 OR .GE. 1.D0'
      CALL XERRWD(MSG,58,23,0,0,0,0,1,RWORK(LEPLI),0.0D0)
      GO TO 750
724   MSG = 'DASPK--  ILLEGAL IWORK VALUE FOR INFO(11) .NE. 0'
      CALL XERRWD(MSG,48,24,0,0,0,0,0,0.0D0,0.0D0)
      GO TO 750
725   MSG = 'DASPK--  ONE OF THE INPUTS FOR INFO(17) = 1 IS ILLEGAL'
      CALL XERRWD(MSG,54,25,0,0,0,0,0,0.0D0,0.0D0)
      GO TO 750
726   MSG = 'DASPK--  ILLEGAL IWORK VALUE FOR INFO(10) .NE. 0'
      CALL XERRWD(MSG,48,26,0,0,0,0,0,0.0D0,0.0D0)
      GO TO 750
727   MSG = 'DASPK--  Y(I) AND IWORK(40+I) (I=I1) INCONSISTENT'
      CALL XERRWD(MSG,49,27,0,1,IRET,0,0,0.0D0,0.0D0)
      GO TO 750
750   IF(INFO(1).EQ.-1) GO TO 760
      INFO(1)=-1
      IDID=-33
      RETURN
760   MSG = 'DASPK--  REPEATED OCCURRENCES OF ILLEGAL INPUT'
      CALL XERRWD(MSG,46,701,0,0,0,0,0,0.0D0,0.0D0)
770   MSG = 'DASPK--  RUN TERMINATED. APPARENT INFINITE LOOP'
      CALL XERRWD(MSG,47,702,1,0,0,0,0,0.0D0,0.0D0)
      RETURN
C
C------END OF SUBROUTINE DDASPK-----------------------------------------
      END

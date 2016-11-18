      SUBROUTINE DDASSL (RES, NEQ, T, Y, YPRIME, TOUT, INFO, RTOL, ATOL,
     +   IDID, RWORK, LRW, IWORK, LIW, RPAR, IPAR, JAC)
C***BEGIN PROLOGUE  DDASSL
C***PURPOSE  This code solves a system of differential/algebraic
C            equations of the form G(T,Y,YPRIME) = 0.
C***LIBRARY   SLATEC (DASSL)
C***CATEGORY  I1A2
C***TYPE      DOUBLE PRECISION (SDASSL-S, DDASSL-D)
C***KEYWORDS  DIFFERENTIAL/ALGEBRAIC, BACKWARD DIFFERENTIATION FORMULAS,
C             IMPLICIT DIFFERENTIAL SYSTEMS
C***AUTHOR  PETZOLD, LINDA R., (LLNL)
C             COMPUTING AND MATHEMATICS RESEARCH DIVISION
C             LAWRENCE LIVERMORE NATIONAL LABORATORY
C             L - 316, P.O. BOX 808,
C             LIVERMORE, CA.    94550
C***DESCRIPTION
C
C *Usage:
C
C      EXTERNAL RES, JAC
C      INTEGER NEQ, INFO(N), IDID, LRW, LIW, IWORK(LIW), IPAR
C      DOUBLE PRECISION T, Y(NEQ), YPRIME(NEQ), TOUT, RTOL, ATOL,
C     *   RWORK(LRW), RPAR
C
C      CALL DDASSL (RES, NEQ, T, Y, YPRIME, TOUT, INFO, RTOL, ATOL,
C     *   IDID, RWORK, LRW, IWORK, LIW, RPAR, IPAR, JAC)
C
C
C *Arguments:
C  (In the following, all real arrays should be type DOUBLE PRECISION.)
C
C  RES:EXT     This is a subroutine which you provide to define the
C              differential/algebraic system.
C
C  NEQ:IN      This is the number of equations to be solved.
C
C  T:INOUT     This is the current value of the independent variable.
C
C  Y(*):INOUT  This array contains the solution components at T.
C
C  YPRIME(*):INOUT  This array contains the derivatives of the solution
C              components at T.
C
C  TOUT:IN     This is a point at which a solution is desired.
C
C  INFO(N):IN  The basic task of the code is to solve the system from T
C              to TOUT and return an answer at TOUT.  INFO is an integer
C              array which is used to communicate exactly how you want
C              this task to be carried out.  (See below for details.)
C              N must be greater than or equal to 15.
C
C  RTOL,ATOL:INOUT  These quantities represent relative and absolute
C              error tolerances which you provide to indicate how
C              accurately you wish the solution to be computed.  You
C              may choose them to be both scalars or else both vectors.
C              Caution:  In Fortran 77, a scalar is not the same as an
C                        array of length 1.  Some compilers may object
C                        to using scalars for RTOL,ATOL.
C
C  IDID:OUT    This scalar quantity is an indicator reporting what the
C              code did.  You must monitor this integer variable to
C              decide  what action to take next.
C
C  RWORK:WORK  A real work array of length LRW which provides the
C              code with needed storage space.
C
C  LRW:IN      The length of RWORK.  (See below for required length.)
C
C  IWORK:WORK  An integer work array of length LIW which probides the
C              code with needed storage space.
C
C  LIW:IN      The length of IWORK.  (See below for required length.)
C
C  RPAR,IPAR:IN  These are real and integer parameter arrays which
C              you can use for communication between your calling
C              program and the RES subroutine (and the JAC subroutine)
C
C  JAC:EXT     This is the name of a subroutine which you may choose
C              to provide for defining a matrix of partial derivatives
C              described below.
C
C  Quantities which may be altered by DDASSL are:
C     T, Y(*), YPRIME(*), INFO(1), RTOL, ATOL,
C     IDID, RWORK(*) AND IWORK(*)
C
C *Description
C
C  Subroutine DDASSL uses the backward differentiation formulas of
C  orders one through five to solve a system of the above form for Y and
C  YPRIME.  Values for Y and YPRIME at the initial time must be given as
C  input.  These values must be consistent, (that is, if T,Y,YPRIME are
C  the given initial values, they must satisfy G(T,Y,YPRIME) = 0.).  The
C  subroutine solves the system from T to TOUT.  It is easy to continue
C  the solution to get results at additional TOUT.  This is the interval
C  mode of operation.  Intermediate results can also be obtained easily
C  by using the intermediate-output capability.
C
C  The following detailed description is divided into subsections:
C    1. Input required for the first call to DDASSL.
C    2. Output after any return from DDASSL.
C    3. What to do to continue the integration.
C    4. Error messages.
C
C
C  -------- INPUT -- WHAT TO DO ON THE FIRST CALL TO DDASSL ------------
C
C  The first call of the code is defined to be the start of each new
C  problem. Read through the descriptions of all the following items,
C  provide sufficient storage space for designated arrays, set
C  appropriate variables for the initialization of the problem, and
C  give information about how you want the problem to be solved.
C
C
C  RES -- Provide a subroutine of the form
C             SUBROUTINE RES(T,Y,YPRIME,DELTA,IRES,RPAR,IPAR)
C         to define the system of differential/algebraic
C         equations which is to be solved. For the given values
C         of T,Y and YPRIME, the subroutine should
C         return the residual of the defferential/algebraic
C         system
C             DELTA = G(T,Y,YPRIME)
C         (DELTA(*) is a vector of length NEQ which is
C         output for RES.)
C
C         Subroutine RES must not alter T,Y or YPRIME.
C         You must declare the name RES in an external
C         statement in your program that calls DDASSL.
C         You must dimension Y,YPRIME and DELTA in RES.
C
C         IRES is an integer flag which is always equal to
C         zero on input. Subroutine RES should alter IRES
C         only if it encounters an illegal value of Y or
C         a stop condition. Set IRES = -1 if an input value
C         is illegal, and DDASSL will try to solve the problem
C         without getting IRES = -1. If IRES = -2, DDASSL
C         will return control to the calling program
C         with IDID = -11.
C
C         RPAR and IPAR are real and integer parameter arrays which
C         you can use for communication between your calling program
C         and subroutine RES. They are not altered by DDASSL. If you
C         do not need RPAR or IPAR, ignore these parameters by treat-
C         ing them as dummy arguments. If you do choose to use them,
C         dimension them in your calling program and in RES as arrays
C         of appropriate length.
C
C  NEQ -- Set it to the number of differential equations.
C         (NEQ .GE. 1)
C
C  T -- Set it to the initial point of the integration.
C         T must be defined as a variable.
C
C  Y(*) -- Set this vector to the initial values of the NEQ solution
C         components at the initial point. You must dimension Y of
C         length at least NEQ in your calling program.
C
C  YPRIME(*) -- Set this vector to the initial values of the NEQ
C         first derivatives of the solution components at the initial
C         point.  You must dimension YPRIME at least NEQ in your
C         calling program. If you do not know initial values of some
C         of the solution components, see the explanation of INFO(11).
C
C  TOUT -- Set it to the first point at which a solution
C         is desired. You can not take TOUT = T.
C         integration either forward in T (TOUT .GT. T) or
C         backward in T (TOUT .LT. T) is permitted.
C
C         The code advances the solution from T to TOUT using
C         step sizes which are automatically selected so as to
C         achieve the desired accuracy. If you wish, the code will
C         return with the solution and its derivative at
C         intermediate steps (intermediate-output mode) so that
C         you can monitor them, but you still must provide TOUT in
C         accord with the basic aim of the code.
C
C         The first step taken by the code is a critical one
C         because it must reflect how fast the solution changes near
C         the initial point. The code automatically selects an
C         initial step size which is practically always suitable for
C         the problem. By using the fact that the code will not step
C         past TOUT in the first step, you could, if necessary,
C         restrict the length of the initial step size.
C
C         For some problems it may not be permissible to integrate
C         past a point TSTOP because a discontinuity occurs there
C         or the solution or its derivative is not defined beyond
C         TSTOP. When you have declared a TSTOP point (SEE INFO(4)
C         and RWORK(1)), you have told the code not to integrate
C         past TSTOP. In this case any TOUT beyond TSTOP is invalid
C         input.
C
C  INFO(*) -- Use the INFO array to give the code more details about
C         how you want your problem solved.  This array should be
C         dimensioned of length 15, though DDASSL uses only the first
C         eleven entries.  You must respond to all of the following
C         items, which are arranged as questions.  The simplest use
C         of the code corresponds to answering all questions as yes,
C         i.e. setting all entries of INFO to 0.
C
C       INFO(1) - This parameter enables the code to initialize
C              itself. You must set it to indicate the start of every
C              new problem.
C
C          **** Is this the first call for this problem ...
C                Yes - Set INFO(1) = 0
C                 No - Not applicable here.
C                      See below for continuation calls.  ****
C
C       INFO(2) - How much accuracy you want of your solution
C              is specified by the error tolerances RTOL and ATOL.
C              The simplest use is to take them both to be scalars.
C              To obtain more flexibility, they can both be vectors.
C              The code must be told your choice.
C
C          **** Are both error tolerances RTOL, ATOL scalars ...
C                Yes - Set INFO(2) = 0
C                      and input scalars for both RTOL and ATOL
C                 No - Set INFO(2) = 1
C                      and input arrays for both RTOL and ATOL ****
C
C       INFO(3) - The code integrates from T in the direction
C              of TOUT by steps. If you wish, it will return the
C              computed solution and derivative at the next
C              intermediate step (the intermediate-output mode) or
C              TOUT, whichever comes first. This is a good way to
C              proceed if you want to see the behavior of the solution.
C              If you must have solutions at a great many specific
C              TOUT points, this code will compute them efficiently.
C
C          **** Do you want the solution only at
C                TOUT (and not at the next intermediate step) ...
C                 Yes - Set INFO(3) = 0
C                  No - Set INFO(3) = 1 ****
C
C       INFO(4) - To handle solutions at a great many specific
C              values TOUT efficiently, this code may integrate past
C              TOUT and interpolate to obtain the result at TOUT.
C              Sometimes it is not possible to integrate beyond some
C              point TSTOP because the equation changes there or it is
C              not defined past TSTOP. Then you must tell the code
C              not to go past.
C
C           **** Can the integration be carried out without any
C                restrictions on the independent variable T ...
C                 Yes - Set INFO(4)=0
C                  No - Set INFO(4)=1
C                       and define the stopping point TSTOP by
C                       setting RWORK(1)=TSTOP ****
C
C       INFO(5) - To solve differential/algebraic problems it is
C              necessary to use a matrix of partial derivatives of the
C              system of differential equations. If you do not
C              provide a subroutine to evaluate it analytically (see
C              description of the item JAC in the call list), it will
C              be approximated by numerical differencing in this code.
C              although it is less trouble for you to have the code
C              compute partial derivatives by numerical differencing,
C              the solution will be more reliable if you provide the
C              derivatives via JAC. Sometimes numerical differencing
C              is cheaper than evaluating derivatives in JAC and
C              sometimes it is not - this depends on your problem.
C
C           **** Do you want the code to evaluate the partial
C                derivatives automatically by numerical differences ...
C                   Yes - Set INFO(5)=0
C                    No - Set INFO(5)=1
C                  and provide subroutine JAC for evaluating the
C                  matrix of partial derivatives ****
C
C       INFO(6) - DDASSL will perform much better if the matrix of
C              partial derivatives, DG/DY + CJ*DG/DYPRIME,
C              (here CJ is a scalar determined by DDASSL)
C              is banded and the code is told this. In this
C              case, the storage needed will be greatly reduced,
C              numerical differencing will be performed much cheaper,
C              and a number of important algorithms will execute much
C              faster. The differential equation is said to have
C              half-bandwidths ML (lower) and MU (upper) if equation i
C              involves only unknowns Y(J) with
C                             I-ML .LE. J .LE. I+MU
C              for all I=1,2,...,NEQ. Thus, ML and MU are the widths
C              of the lower and upper parts of the band, respectively,
C              with the main diagonal being excluded. If you do not
C              indicate that the equation has a banded matrix of partial
C              derivatives, the code works with a full matrix of NEQ**2
C              elements (stored in the conventional way). Computations
C              with banded matrices cost less time and storage than with
C              full matrices if 2*ML+MU .LT. NEQ. If you tell the
C              code that the matrix of partial derivatives has a banded
C              structure and you want to provide subroutine JAC to
C              compute the partial derivatives, then you must be careful
C              to store the elements of the matrix in the special form
C              indicated in the description of JAC.
C
C          **** Do you want to solve the problem using a full
C               (dense) matrix (and not a special banded
C               structure) ...
C                Yes - Set INFO(6)=0
C                 No - Set INFO(6)=1
C                       and provide the lower (ML) and upper (MU)
C                       bandwidths by setting
C                       IWORK(1)=ML
C                       IWORK(2)=MU ****
C
C
C        INFO(7) -- You can specify a maximum (absolute value of)
C              stepsize, so that the code
C              will avoid passing over very
C              large regions.
C
C          ****  Do you want the code to decide
C                on its own maximum stepsize?
C                Yes - Set INFO(7)=0
C                 No - Set INFO(7)=1
C                      and define HMAX by setting
C                      RWORK(2)=HMAX ****
C
C        INFO(8) -- Differential/algebraic problems
C              may occaisionally suffer from
C              severe scaling difficulties on the
C              first step. If you know a great deal
C              about the scaling of your problem, you can
C              help to alleviate this problem by
C              specifying an initial stepsize HO.
C
C          ****  Do you want the code to define
C                its own initial stepsize?
C                Yes - Set INFO(8)=0
C                 No - Set INFO(8)=1
C                      and define HO by setting
C                      RWORK(3)=HO ****
C
C        INFO(9) -- If storage is a severe problem,
C              you can save some locations by
C              restricting the maximum order MAXORD.
C              the default value is 5. for each
C              order decrease below 5, the code
C              requires NEQ fewer locations, however
C              it is likely to be slower. In any
C              case, you must have 1 .LE. MAXORD .LE. 5
C          ****  Do you want the maximum order to
C                default to 5?
C                Yes - Set INFO(9)=0
C                 No - Set INFO(9)=1
C                      and define MAXORD by setting
C                      IWORK(3)=MAXORD ****
C
C        INFO(10) --If you know that the solutions to your equations
C               will always be nonnegative, it may help to set this
C               parameter. However, it is probably best to
C               try the code without using this option first,
C               and only to use this option if that doesn't
C               work very well.
C           ****  Do you want the code to solve the problem without
C                 invoking any special nonnegativity constraints?
C                  Yes - Set INFO(10)=0
C                   No - Set INFO(10)=1
C
C        INFO(11) --DDASSL normally requires the initial T,
C               Y, and YPRIME to be consistent. That is,
C               you must have G(T,Y,YPRIME) = 0 at the initial
C               time. If you do not know the initial
C               derivative precisely, you can let DDASSL try
C               to compute it.
C          ****   Are the initialHE INITIAL T, Y, YPRIME consistent?
C                 Yes - Set INFO(11) = 0
C                  No - Set INFO(11) = 1,
C                       and set YPRIME to an initial approximation
C                       to YPRIME.  (If you have no idea what
C                       YPRIME should be, set it to zero. Note
C                       that the initial Y should be such
C                       that there must exist a YPRIME so that
C                       G(T,Y,YPRIME) = 0.)
C
C  RTOL, ATOL -- You must assign relative (RTOL) and absolute (ATOL
C         error tolerances to tell the code how accurately you
C         want the solution to be computed.  They must be defined
C         as variables because the code may change them.  You
C         have two choices --
C               Both RTOL and ATOL are scalars. (INFO(2)=0)
C               Both RTOL and ATOL are vectors. (INFO(2)=1)
C         in either case all components must be non-negative.
C
C         The tolerances are used by the code in a local error
C         test at each step which requires roughly that
C               ABS(LOCAL ERROR) .LE. RTOL*ABS(Y)+ATOL
C         for each vector component.
C         (More specifically, a root-mean-square norm is used to
C         measure the size of vectors, and the error test uses the
C         magnitude of the solution at the beginning of the step.)
C
C         The true (global) error is the difference between the
C         true solution of the initial value problem and the
C         computed approximation.  Practically all present day
C         codes, including this one, control the local error at
C         each step and do not even attempt to control the global
C         error directly.
C         Usually, but not always, the true accuracy of the
C         computed Y is comparable to the error tolerances. This
C         code will usually, but not always, deliver a more
C         accurate solution if you reduce the tolerances and
C         integrate again.  By comparing two such solutions you
C         can get a fairly reliable idea of the true error in the
C         solution at the bigger tolerances.
C
C         Setting ATOL=0. results in a pure relative error test on
C         that component.  Setting RTOL=0. results in a pure
C         absolute error test on that component.  A mixed test
C         with non-zero RTOL and ATOL corresponds roughly to a
C         relative error test when the solution component is much
C         bigger than ATOL and to an absolute error test when the
C         solution component is smaller than the threshhold ATOL.
C
C         The code will not attempt to compute a solution at an
C         accuracy unreasonable for the machine being used.  It will
C         advise you if you ask for too much accuracy and inform
C         you as to the maximum accuracy it believes possible.
C
C  RWORK(*) --  Dimension this real work array of length LRW in your
C         calling program.
C
C  LRW -- Set it to the declared length of the RWORK array.
C               You must have
C                    LRW .GE. 40+(MAXORD+4)*NEQ+NEQ**2
C               for the full (dense) JACOBIAN case (when INFO(6)=0), or
C                    LRW .GE. 40+(MAXORD+4)*NEQ+(2*ML+MU+1)*NEQ
C               for the banded user-defined JACOBIAN case
C               (when INFO(5)=1 and INFO(6)=1), or
C                     LRW .GE. 40+(MAXORD+4)*NEQ+(2*ML+MU+1)*NEQ
C                           +2*(NEQ/(ML+MU+1)+1)
C               for the banded finite-difference-generated JACOBIAN case
C               (when INFO(5)=0 and INFO(6)=1)
C
C  IWORK(*) --  Dimension this integer work array of length LIW in
C         your calling program.
C
C  LIW -- Set it to the declared length of the IWORK array.
C               You must have LIW .GE. 21+NEQ
C
C  RPAR, IPAR -- These are parameter arrays, of real and integer
C         type, respectively.  You can use them for communication
C         between your program that calls DDASSL and the
C         RES subroutine (and the JAC subroutine).  They are not
C         altered by DDASSL.  If you do not need RPAR or IPAR,
C         ignore these parameters by treating them as dummy
C         arguments.  If you do choose to use them, dimension
C         them in your calling program and in RES (and in JAC)
C         as arrays of appropriate length.
C
C  JAC -- If you have set INFO(5)=0, you can ignore this parameter
C         by treating it as a dummy argument.  Otherwise, you must
C         provide a subroutine of the form
C             SUBROUTINE JAC(T,Y,YPRIME,PD,CJ,RPAR,IPAR)
C         to define the matrix of partial derivatives
C             PD=DG/DY+CJ*DG/DYPRIME
C         CJ is a scalar which is input to JAC.
C         For the given values of T,Y,YPRIME, the
C         subroutine must evaluate the non-zero partial
C         derivatives for each equation and each solution
C         component, and store these values in the
C         matrix PD.  The elements of PD are set to zero
C         before each call to JAC so only non-zero elements
C         need to be defined.
C
C         Subroutine JAC must not alter T,Y,(*),YPRIME(*), or CJ.
C         You must declare the name JAC in an EXTERNAL statement in
C         your program that calls DDASSL.  You must dimension Y,
C         YPRIME and PD in JAC.
C
C         The way you must store the elements into the PD matrix
C         depends on the structure of the matrix which you
C         indicated by INFO(6).
C               *** INFO(6)=0 -- Full (dense) matrix ***
C                   Give PD a first dimension of NEQ.
C                   When you evaluate the (non-zero) partial derivative
C                   of equation I with respect to variable J, you must
C                   store it in PD according to
C                   PD(I,J) = "DG(I)/DY(J)+CJ*DG(I)/DYPRIME(J)"
C               *** INFO(6)=1 -- Banded JACOBIAN with ML lower and MU
C                   upper diagonal bands (refer to INFO(6) description
C                   of ML and MU) ***
C                   Give PD a first dimension of 2*ML+MU+1.
C                   when you evaluate the (non-zero) partial derivative
C                   of equation I with respect to variable J, you must
C                   store it in PD according to
C                   IROW = I - J + ML + MU + 1
C                   PD(IROW,J) = "DG(I)/DY(J)+CJ*DG(I)/DYPRIME(J)"
C
C         RPAR and IPAR are real and integer parameter arrays
C         which you can use for communication between your calling
C         program and your JACOBIAN subroutine JAC. They are not
C         altered by DDASSL. If you do not need RPAR or IPAR,
C         ignore these parameters by treating them as dummy
C         arguments. If you do choose to use them, dimension
C         them in your calling program and in JAC as arrays of
C         appropriate length.
C
C
C  OPTIONALLY REPLACEABLE NORM ROUTINE:
C
C     DDASSL uses a weighted norm DDANRM to measure the size
C     of vectors such as the estimated error in each step.
C     A FUNCTION subprogram
C       DOUBLE PRECISION FUNCTION DDANRM(NEQ,V,WT,RPAR,IPAR)
C       DIMENSION V(NEQ),WT(NEQ)
C     is used to define this norm. Here, V is the vector
C     whose norm is to be computed, and WT is a vector of
C     weights.  A DDANRM routine has been included with DDASSL
C     which computes the weighted root-mean-square norm
C     given by
C       DDANRM=SQRT((1/NEQ)*SUM(V(I)/WT(I))**2)
C     this norm is suitable for most problems. In some
C     special cases, it may be more convenient and/or
C     efficient to define your own norm by writing a function
C     subprogram to be called instead of DDANRM. This should,
C     however, be attempted only after careful thought and
C     consideration.
C
C
C  -------- OUTPUT -- AFTER ANY RETURN FROM DDASSL ---------------------
C
C  The principal aim of the code is to return a computed solution at
C  TOUT, although it is also possible to obtain intermediate results
C  along the way. To find out whether the code achieved its goal
C  or if the integration process was interrupted before the task was
C  completed, you must check the IDID parameter.
C
C
C  T -- The solution was successfully advanced to the
C               output value of T.
C
C  Y(*) -- Contains the computed solution approximation at T.
C
C  YPRIME(*) -- Contains the computed derivative
C               approximation at T.
C
C  IDID -- Reports what the code did.
C
C                     *** Task completed ***
C                Reported by positive values of IDID
C
C           IDID = 1 -- A step was successfully taken in the
C                   intermediate-output mode. The code has not
C                   yet reached TOUT.
C
C           IDID = 2 -- The integration to TSTOP was successfully
C                   completed (T=TSTOP) by stepping exactly to TSTOP.
C
C           IDID = 3 -- The integration to TOUT was successfully
C                   completed (T=TOUT) by stepping past TOUT.
C                   Y(*) is obtained by interpolation.
C                   YPRIME(*) is obtained by interpolation.
C
C                    *** Task interrupted ***
C                Reported by negative values of IDID
C
C           IDID = -1 -- A large amount of work has been expended.
C                   (About 500 steps)
C
C           IDID = -2 -- The error tolerances are too stringent.
C
C           IDID = -3 -- The local error test cannot be satisfied
C                   because you specified a zero component in ATOL
C                   and the corresponding computed solution
C                   component is zero. Thus, a pure relative error
C                   test is impossible for this component.
C
C           IDID = -6 -- DDASSL had repeated error test
C                   failures on the last attempted step.
C
C           IDID = -7 -- The corrector could not converge.
C
C           IDID = -8 -- The matrix of partial derivatives
C                   is singular.
C
C           IDID = -9 -- The corrector could not converge.
C                   there were repeated error test failures
C                   in this step.
C
C           IDID =-10 -- The corrector could not converge
C                   because IRES was equal to minus one.
C
C           IDID =-11 -- IRES equal to -2 was encountered
C                   and control is being returned to the
C                   calling program.
C
C           IDID =-12 -- DDASSL failed to compute the initial
C                   YPRIME.
C
C
C
C           IDID = -13,..,-32 -- Not applicable for this code
C
C                    *** Task terminated ***
C                Reported by the value of IDID=-33
C
C           IDID = -33 -- The code has encountered trouble from which
C                   it cannot recover. A message is printed
C                   explaining the trouble and control is returned
C                   to the calling program. For example, this occurs
C                   when invalid input is detected.
C
C  RTOL, ATOL -- These quantities remain unchanged except when
C               IDID = -2. In this case, the error tolerances have been
C               increased by the code to values which are estimated to
C               be appropriate for continuing the integration. However,
C               the reported solution at T was obtained using the input
C               values of RTOL and ATOL.
C
C  RWORK, IWORK -- Contain information which is usually of no
C               interest to the user but necessary for subsequent calls.
C               However, you may find use for
C
C               RWORK(3)--Which contains the step size H to be
C                       attempted on the next step.
C
C               RWORK(4)--Which contains the current value of the
C                       independent variable, i.e., the farthest point
C                       integration has reached. This will be different
C                       from T only when interpolation has been
C                       performed (IDID=3).
C
C               RWORK(7)--Which contains the stepsize used
C                       on the last successful step.
C
C               IWORK(7)--Which contains the order of the method to
C                       be attempted on the next step.
C
C               IWORK(8)--Which contains the order of the method used
C                       on the last step.
C
C               IWORK(11)--Which contains the number of steps taken so
C                        far.
C
C               IWORK(12)--Which contains the number of calls to RES
C                        so far.
C
C               IWORK(13)--Which contains the number of evaluations of
C                        the matrix of partial derivatives needed so
C                        far.
C
C               IWORK(14)--Which contains the total number
C                        of error test failures so far.
C
C               IWORK(15)--Which contains the total number
C                        of convergence test failures so far.
C                        (includes singular iteration matrix
C                        failures.)
C
C
C  -------- INPUT -- WHAT TO DO TO CONTINUE THE INTEGRATION ------------
C                    (CALLS AFTER THE FIRST)
C
C  This code is organized so that subsequent calls to continue the
C  integration involve little (if any) additional effort on your
C  part. You must monitor the IDID parameter in order to determine
C  what to do next.
C
C  Recalling that the principal task of the code is to integrate
C  from T to TOUT (the interval mode), usually all you will need
C  to do is specify a new TOUT upon reaching the current TOUT.
C
C  Do not alter any quantity not specifically permitted below,
C  in particular do not alter NEQ,T,Y(*),YPRIME(*),RWORK(*),IWORK(*)
C  or the differential equation in subroutine RES. Any such
C  alteration constitutes a new problem and must be treated as such,
C  i.e., you must start afresh.
C
C  You cannot change from vector to scalar error control or vice
C  versa (INFO(2)), but you can change the size of the entries of
C  RTOL, ATOL. Increasing a tolerance makes the equation easier
C  to integrate. Decreasing a tolerance will make the equation
C  harder to integrate and should generally be avoided.
C
C  You can switch from the intermediate-output mode to the
C  interval mode (INFO(3)) or vice versa at any time.
C
C  If it has been necessary to prevent the integration from going
C  past a point TSTOP (INFO(4), RWORK(1)), keep in mind that the
C  code will not integrate to any TOUT beyond the currently
C  specified TSTOP. Once TSTOP has been reached you must change
C  the value of TSTOP or set INFO(4)=0. You may change INFO(4)
C  or TSTOP at any time but you must supply the value of TSTOP in
C  RWORK(1) whenever you set INFO(4)=1.
C
C  Do not change INFO(5), INFO(6), IWORK(1), or IWORK(2)
C  unless you are going to restart the code.
C
C                 *** Following a completed task ***
C  If
C     IDID = 1, call the code again to continue the integration
C                  another step in the direction of TOUT.
C
C     IDID = 2 or 3, define a new TOUT and call the code again.
C                  TOUT must be different from T. You cannot change
C                  the direction of integration without restarting.
C
C                 *** Following an interrupted task ***
C               To show the code that you realize the task was
C               interrupted and that you want to continue, you
C               must take appropriate action and set INFO(1) = 1
C  If
C    IDID = -1, The code has taken about 500 steps.
C                  If you want to continue, set INFO(1) = 1 and
C                  call the code again. An additional 500 steps
C                  will be allowed.
C
C    IDID = -2, The error tolerances RTOL, ATOL have been
C                  increased to values the code estimates appropriate
C                  for continuing. You may want to change them
C                  yourself. If you are sure you want to continue
C                  with relaxed error tolerances, set INFO(1)=1 and
C                  call the code again.
C
C    IDID = -3, A solution component is zero and you set the
C                  corresponding component of ATOL to zero. If you
C                  are sure you want to continue, you must first
C                  alter the error criterion to use positive values
C                  for those components of ATOL corresponding to zero
C                  solution components, then set INFO(1)=1 and call
C                  the code again.
C
C    IDID = -4,-5  --- Cannot occur with this code.
C
C    IDID = -6, Repeated error test failures occurred on the
C                  last attempted step in DDASSL. A singularity in the
C                  solution may be present. If you are absolutely
C                  certain you want to continue, you should restart
C                  the integration. (Provide initial values of Y and
C                  YPRIME which are consistent)
C
C    IDID = -7, Repeated convergence test failures occurred
C                  on the last attempted step in DDASSL. An inaccurate
C                  or ill-conditioned JACOBIAN may be the problem. If
C                  you are absolutely certain you want to continue, you
C                  should restart the integration.
C
C    IDID = -8, The matrix of partial derivatives is singular.
C                  Some of your equations may be redundant.
C                  DDASSL cannot solve the problem as stated.
C                  It is possible that the redundant equations
C                  could be removed, and then DDASSL could
C                  solve the problem. It is also possible
C                  that a solution to your problem either
C                  does not exist or is not unique.
C
C    IDID = -9, DDASSL had multiple convergence test
C                  failures, preceeded by multiple error
C                  test failures, on the last attempted step.
C                  It is possible that your problem
C                  is ill-posed, and cannot be solved
C                  using this code. Or, there may be a
C                  discontinuity or a singularity in the
C                  solution. If you are absolutely certain
C                  you want to continue, you should restart
C                  the integration.
C
C    IDID =-10, DDASSL had multiple convergence test failures
C                  because IRES was equal to minus one.
C                  If you are absolutely certain you want
C                  to continue, you should restart the
C                  integration.
C
C    IDID =-11, IRES=-2 was encountered, and control is being
C                  returned to the calling program.
C
C    IDID =-12, DDASSL failed to compute the initial YPRIME.
C                  This could happen because the initial
C                  approximation to YPRIME was not very good, or
C                  if a YPRIME consistent with the initial Y
C                  does not exist. The problem could also be caused
C                  by an inaccurate or singular iteration matrix.
C
C    IDID = -13,..,-32  --- Cannot occur with this code.
C
C
C                 *** Following a terminated task ***
C
C  If IDID= -33, you cannot continue the solution of this problem.
C                  An attempt to do so will result in your
C                  run being terminated.
C
C
C  -------- ERROR MESSAGES ---------------------------------------------
C
C      The SLATEC error print routine XERMSG is called in the event of
C   unsuccessful completion of a task.  Most of these are treated as
C   "recoverable errors", which means that (unless the user has directed
C   otherwise) control will be returned to the calling program for
C   possible action after the message has been printed.
C
C   In the event of a negative value of IDID other than -33, an appro-
C   priate message is printed and the "error number" printed by XERMSG
C   is the value of IDID.  There are quite a number of illegal input
C   errors that can lead to a returned value IDID=-33.  The conditions
C   and their printed "error numbers" are as follows:
C
C   Error number       Condition
C
C        1       Some element of INFO vector is not zero or one.
C        2       NEQ .le. 0
C        3       MAXORD not in range.
C        4       LRW is less than the required length for RWORK.
C        5       LIW is less than the required length for IWORK.
C        6       Some element of RTOL is .lt. 0
C        7       Some element of ATOL is .lt. 0
C        8       All elements of RTOL and ATOL are zero.
C        9       INFO(4)=1 and TSTOP is behind TOUT.
C       10       HMAX .lt. 0.0
C       11       TOUT is behind T.
C       12       INFO(8)=1 and H0=0.0
C       13       Some element of WT is .le. 0.0
C       14       TOUT is too close to T to start integration.
C       15       INFO(4)=1 and TSTOP is behind T.
C       16       --( Not used in this version )--
C       17       ML illegal.  Either .lt. 0 or .gt. NEQ
C       18       MU illegal.  Either .lt. 0 or .gt. NEQ
C       19       TOUT = T.
C
C   If DDASSL is called again without any action taken to remove the
C   cause of an unsuccessful return, XERMSG will be called with a fatal
C   error flag, which will cause unconditional termination of the
C   program.  There are two such fatal errors:
C
C   Error number -998:  The last step was terminated with a negative
C       value of IDID other than -33, and no appropriate action was
C       taken.
C
C   Error number -999:  The previous call was terminated because of
C       illegal input (IDID=-33) and there is illegal input in the
C       present call, as well.  (Suspect infinite loop.)
C
C  ---------------------------------------------------------------------
C
C***REFERENCES  A DESCRIPTION OF DASSL: A DIFFERENTIAL/ALGEBRAIC
C                 SYSTEM SOLVER, L. R. PETZOLD, SAND82-8637,
C                 SANDIA NATIONAL LABORATORIES, SEPTEMBER 1982.
C***ROUTINES CALLED  D1MACH, DDAINI, DDANRM, DDASTP, DDATRP, DDAWTS,
C                    XERMSG
C***REVISION HISTORY  (YYMMDD)
C   830315  DATE WRITTEN
C   880387  Code changes made.  All common statements have been
C           replaced by a DATA statement, which defines pointers into
C           RWORK, and PARAMETER statements which define pointers
C           into IWORK.  As well the documentation has gone through
C           grammatical changes.
C   881005  The prologue has been changed to mixed case.
C           The subordinate routines had revision dates changed to
C           this date, although the documentation for these routines
C           is all upper case.  No code changes.
C   890511  Code changes made.  The DATA statement in the declaration
C           section of DDASSL was replaced with a PARAMETER
C           statement.  Also the statement S = 100.D0 was removed
C           from the top of the Newton iteration in DDASTP.
C           The subordinate routines had revision dates changed to
C           this date.
C   890517  The revision date syntax was replaced with the revision
C           history syntax.  Also the "DECK" comment was added to
C           the top of all subroutines.  These changes are consistent
C           with new SLATEC guidelines.
C           The subordinate routines had revision dates changed to
C           this date.  No code changes.
C   891013  Code changes made.
C           Removed all occurrances of FLOAT or DBLE.  All operations
C           are now performed with "mixed-mode" arithmetic.
C           Also, specific function names were replaced with generic
C           function names to be consistent with new SLATEC guidelines.
C           In particular:
C              Replaced DSQRT with SQRT everywhere.
C              Replaced DABS with ABS everywhere.
C              Replaced DMIN1 with MIN everywhere.
C              Replaced MIN0 with MIN everywhere.
C              Replaced DMAX1 with MAX everywhere.
C              Replaced MAX0 with MAX everywhere.
C              Replaced DSIGN with SIGN everywhere.
C           Also replaced REVISION DATE with REVISION HISTORY in all
C           subordinate routines.
C  901004  Miscellaneous changes to prologue to complete conversion
C          to SLATEC 4.0 format.  No code changes.  (F.N.Fritsch)
C  901009  Corrected GAMS classification code and converted subsidiary
C          routines to 4.0 format.  No code changes.  (F.N.Fritsch)
C  901010  Converted XERRWV calls to XERMSG calls.  (R.Clemens,AFWL)
C  901019  Code changes made.
C          Merged SLATEC 4.0 changes with previous changes made
C          by C. Ulrich.  Below is a history of the changes made by
C          C. Ulrich. (Changes in subsidiary routines are implied
C          by this history)
C          891228  Bug was found and repaired inside the DDASSL
C                  and DDAINI routines.  DDAINI was incorrectly
C                  returning the initial T with Y and YPRIME
C                  computed at T+H.  The routine now returns T+H
C                  rather than the initial T.
C                  Cosmetic changes made to DDASTP.
C          900904  Three modifications were made to fix a bug (inside
C                  DDASSL) re interpolation for continuation calls and
C                  cases where TN is very close to TSTOP:
C
C                  1) In testing for whether H is too large, just
C                     compare H to (TSTOP - TN), rather than
C                     (TSTOP - TN) * (1-4*UROUND), and set H to
C                     TSTOP - TN.  This will force DDASTP to step
C                     exactly to TSTOP under certain situations
C                     (i.e. when H returned from DDASTP would otherwise
C                     take TN beyond TSTOP).
C
C                  2) Inside the DDASTP loop, interpolate exactly to
C                     TSTOP if TN is very close to TSTOP (rather than
C                     interpolating to within roundoff of TSTOP).
C
C                  3) Modified IDID description for IDID = 2 to say that
C                     the solution is returned by stepping exactly to
C                     TSTOP, rather than TOUT.  (In some cases the
C                     solution is actually obtained by extrapolating
C                     over a distance near unit roundoff to TSTOP,
C                     but this small distance is deemed acceptable in
C                     these circumstances.)
C   901026  Added explicit declarations for all variables and minor
C           cosmetic changes to prologue, removed unreferenced labels,
C           and improved XERMSG calls.  (FNF)
C   901030  Added ERROR MESSAGES section and reworked other sections to
C           be of more uniform format.  (FNF)
C   910624  Fixed minor bug related to HMAX (five lines ending in
C           statement 526 in DDASSL).   (LRP)
C
C***END PROLOGUE  DDASSL
C
C**End
C
C     Declare arguments.
C
      INTEGER  NEQ, INFO(15), IDID, LRW, IWORK(*), LIW, IPAR(*)
      DOUBLE PRECISION
     *   T, Y(*), YPRIME(*), TOUT, RTOL(*), ATOL(*), RWORK(*),
     *   RPAR(*)
      EXTERNAL  RES, JAC
C
C     Declare externals.
C
      EXTERNAL  D1MACH, DDAINI, DDANRM, DDASTP, DDATRP, DDAWTS, XERMSG
      DOUBLE PRECISION  D1MACH, DDANRM
C
C     Declare local variables.
C
      INTEGER  I, ITEMP, LALPHA, LBETA, LCJ, LCJOLD, LCTF, LDELTA,
     *   LENIW, LENPD, LENRW, LE, LETF, LGAMMA, LH, LHMAX, LHOLD,
     *   LMXSTP, LIPVT,
     *   LJCALC, LK, LKOLD, LIWM, LML, LMTYPE, LMU, LMXORD, LNJE, LNPD,
     *   LNRE, LNS, LNST, LNSTL, LPD, LPHASE, LPHI, LPSI, LROUND, LS,
     *   LSIGMA, LTN, LTSTOP, LWM, LWT, MBAND, MSAVE, MXORD, NPD, NTEMP,
     *   NZFLG
      DOUBLE PRECISION
     *   ATOLI, H, HMAX, HMIN, HO, R, RH, RTOLI, TDIST, TN, TNEXT,
     *   TSTOP, UROUND, YPNORM
      LOGICAL  DONE
C       Auxiliary variables for conversion of values to be included in
C       error messages.
      CHARACTER*8  XERN1, XERN2
      CHARACTER*16 XERN3, XERN4
C
C     SET POINTERS INTO IWORK
      PARAMETER (LML=1, LMU=2, LMXORD=3, LMTYPE=4, LNST=11,
     *  LNRE=12, LNJE=13, LETF=14, LCTF=15, LNPD=16, LMXSTP=21,
     *  LIPVT=22, LJCALC=5, LPHASE=6, LK=7, LKOLD=8,
     *  LNS=9, LNSTL=10, LIWM=1)
C
C     SET RELATIVE OFFSET INTO RWORK
      PARAMETER (NPD=1)
C
C     SET POINTERS INTO RWORK
      PARAMETER (LTSTOP=1, LHMAX=2, LH=3, LTN=4,
     *  LCJ=5, LCJOLD=6, LHOLD=7, LS=8, LROUND=9,
     *  LALPHA=11, LBETA=17, LGAMMA=23,
     *  LPSI=29, LSIGMA=35, LDELTA=41)
C
C***FIRST EXECUTABLE STATEMENT  DDASSL
      IF(INFO(1).NE.0)GO TO 100
C
C-----------------------------------------------------------------------
C     THIS BLOCK IS EXECUTED FOR THE INITIAL CALL ONLY.
C     IT CONTAINS CHECKING OF INPUTS AND INITIALIZATIONS.
C-----------------------------------------------------------------------
C
C     FIRST CHECK INFO ARRAY TO MAKE SURE ALL ELEMENTS OF INFO
C     ARE EITHER ZERO OR ONE.
      DO 10 I=2,11
         IF(INFO(I).NE.0.AND.INFO(I).NE.1)GO TO 701
10       CONTINUE
C
      IF(NEQ.LE.0)GO TO 702
C
C     CHECK AND COMPUTE MAXIMUM ORDER
      MXORD=5
      IF(INFO(9).EQ.0)GO TO 20
         MXORD=IWORK(LMXORD)
         IF(MXORD.LT.1.OR.MXORD.GT.5)GO TO 703
20       IWORK(LMXORD)=MXORD
C
C     COMPUTE MTYPE,LENPD,LENRW.CHECK ML AND MU.
      IF(INFO(6).NE.0)GO TO 40
         LENPD=NEQ**2
         LENRW=40+(IWORK(LMXORD)+4)*NEQ+LENPD
         IF(INFO(5).NE.0)GO TO 30
            IWORK(LMTYPE)=2
            GO TO 60
30          IWORK(LMTYPE)=1
            GO TO 60
40    IF(IWORK(LML).LT.0.OR.IWORK(LML).GE.NEQ)GO TO 717
      IF(IWORK(LMU).LT.0.OR.IWORK(LMU).GE.NEQ)GO TO 718
      LENPD=(2*IWORK(LML)+IWORK(LMU)+1)*NEQ
      IF(INFO(5).NE.0)GO TO 50
         IWORK(LMTYPE)=5
         MBAND=IWORK(LML)+IWORK(LMU)+1
         MSAVE=(NEQ/MBAND)+1
         LENRW=40+(IWORK(LMXORD)+4)*NEQ+LENPD+2*MSAVE
         GO TO 60
50       IWORK(LMTYPE)=4
         LENRW=40+(IWORK(LMXORD)+4)*NEQ+LENPD
C
C     CHECK LENGTHS OF RWORK AND IWORK
60    LENIW=21+NEQ
      IWORK(LNPD)=LENPD
      IF(LRW.LT.LENRW)GO TO 704
      IF(LIW.LT.LENIW)GO TO 705
C
C     CHECK TO SEE THAT TOUT IS DIFFERENT FROM T
      IF(TOUT .EQ. T)GO TO 719
C
C     CHECK HMAX
      IF(INFO(7).EQ.0)GO TO 70
         HMAX=RWORK(LHMAX)
         IF(HMAX.LE.0.0D0)GO TO 710
70    CONTINUE
C
C     CHECK AND COMPUTE MAXIMUM STEPS
      MXSTP=500
      IF(INFO(12).EQ.0)GO TO 80
        MXSTP=IWORK(LMXSTP)
        IF(MXSTP.LT.0)GO TO 716
80      IWORK(LMXSTP)=MXSTP
C
C     INITIALIZE COUNTERS
      IWORK(LNST)=0
      IWORK(LNRE)=0
      IWORK(LNJE)=0
C
      IWORK(LNSTL)=0
      IDID=1
      GO TO 200
C
C-----------------------------------------------------------------------
C     THIS BLOCK IS FOR CONTINUATION CALLS
C     ONLY. HERE WE CHECK INFO(1),AND IF THE
C     LAST STEP WAS INTERRUPTED WE CHECK WHETHER
C     APPROPRIATE ACTION WAS TAKEN.
C-----------------------------------------------------------------------
C
100   CONTINUE
      IF(INFO(1).EQ.1)GO TO 110
      IF(INFO(1).NE.-1)GO TO 701
C
C     IF WE ARE HERE, THE LAST STEP WAS INTERRUPTED
C     BY AN ERROR CONDITION FROM DDASTP,AND
C     APPROPRIATE ACTION WAS NOT TAKEN. THIS
C     IS A FATAL ERROR.
      WRITE (XERN1, '(I8)') IDID
      CALL XERMSG ('SLATEC', 'DDASSL',
     *   'THE LAST STEP TERMINATED WITH A NEGATIVE VALUE OF IDID = ' //
     *   XERN1 // ' AND NO APPROPRIATE ACTION WAS TAKEN.  ' //
     *   'RUN TERMINATED', -998, 2)
      RETURN
110   CONTINUE
      IWORK(LNSTL)=IWORK(LNST)
C
C-----------------------------------------------------------------------
C     THIS BLOCK IS EXECUTED ON ALL CALLS.
C     THE ERROR TOLERANCE PARAMETERS ARE
C     CHECKED, AND THE WORK ARRAY POINTERS
C     ARE SET.
C-----------------------------------------------------------------------
C
200   CONTINUE
C     CHECK RTOL,ATOL
      NZFLG=0
      RTOLI=RTOL(1)
      ATOLI=ATOL(1)
      DO 210 I=1,NEQ
         IF(INFO(2).EQ.1)RTOLI=RTOL(I)
         IF(INFO(2).EQ.1)ATOLI=ATOL(I)
         IF(RTOLI.GT.0.0D0.OR.ATOLI.GT.0.0D0)NZFLG=1
         IF(RTOLI.LT.0.0D0)GO TO 706
         IF(ATOLI.LT.0.0D0)GO TO 707
210      CONTINUE
      IF(NZFLG.EQ.0)GO TO 708
C
C     SET UP RWORK STORAGE.IWORK STORAGE IS FIXED
C     IN DATA STATEMENT.
      LE=LDELTA+NEQ
      LWT=LE+NEQ
      LPHI=LWT+NEQ
      LPD=LPHI+(IWORK(LMXORD)+1)*NEQ
      LWM=LPD
      NTEMP=NPD+IWORK(LNPD)
      IF(INFO(1).EQ.1)GO TO 400
C
C-----------------------------------------------------------------------
C     THIS BLOCK IS EXECUTED ON THE INITIAL CALL
C     ONLY. SET THE INITIAL STEP SIZE, AND
C     THE ERROR WEIGHT VECTOR, AND PHI.
C     COMPUTE INITIAL YPRIME, IF NECESSARY.
C-----------------------------------------------------------------------
C
      TN=T
      IDID=1
C
C     SET ERROR WEIGHT VECTOR WT
      CALL DDAWTS(NEQ,INFO(2),RTOL,ATOL,Y,RWORK(LWT),RPAR,IPAR)
      DO 305 I = 1,NEQ
         IF(RWORK(LWT+I-1).LE.0.0D0) GO TO 713
305      CONTINUE
C
C     COMPUTE UNIT ROUNDOFF AND HMIN
      UROUND = D1MACH(4)
      RWORK(LROUND) = UROUND
      HMIN = 4.0D0*UROUND*MAX(ABS(T),ABS(TOUT))
C
C     CHECK INITIAL INTERVAL TO SEE THAT IT IS LONG ENOUGH
      TDIST = ABS(TOUT - T)
      IF(TDIST .LT. HMIN) GO TO 714
C
C     CHECK HO, IF THIS WAS INPUT
      IF (INFO(8) .EQ. 0) GO TO 310
         HO = RWORK(LH)
         IF ((TOUT - T)*HO .LT. 0.0D0) GO TO 711
         IF (HO .EQ. 0.0D0) GO TO 712
         GO TO 320
310    CONTINUE
C
C     COMPUTE INITIAL STEPSIZE, TO BE USED BY EITHER
C     DDASTP OR DDAINI, DEPENDING ON INFO(11)
      HO = 0.001D0*TDIST
      YPNORM = DDANRM(NEQ,YPRIME,RWORK(LWT),RPAR,IPAR)
      IF (YPNORM .GT. 0.5D0/HO) HO = 0.5D0/YPNORM
      HO = SIGN(HO,TOUT-T)
C     ADJUST HO IF NECESSARY TO MEET HMAX BOUND
320   IF (INFO(7) .EQ. 0) GO TO 330
         RH = ABS(HO)/RWORK(LHMAX)
         IF (RH .GT. 1.0D0) HO = HO/RH
C     COMPUTE TSTOP, IF APPLICABLE
330   IF (INFO(4) .EQ. 0) GO TO 340
         TSTOP = RWORK(LTSTOP)
         IF ((TSTOP - T)*HO .LT. 0.0D0) GO TO 715
         IF ((T + HO - TSTOP)*HO .GT. 0.0D0) HO = TSTOP - T
         IF ((TSTOP - TOUT)*HO .LT. 0.0D0) GO TO 709
C
C     COMPUTE INITIAL DERIVATIVE, UPDATING TN AND Y, IF APPLICABLE
340   IF (INFO(11) .EQ. 0) GO TO 350
      CALL DDAINI(TN,Y,YPRIME,NEQ,
     *  RES,JAC,HO,RWORK(LWT),IDID,RPAR,IPAR,
     *  RWORK(LPHI),RWORK(LDELTA),RWORK(LE),
     *  RWORK(LWM),IWORK(LIWM),HMIN,RWORK(LROUND),
     *  INFO(10),NTEMP)
      IF (IDID .LT. 0) GO TO 390
C
C     LOAD H WITH HO.  STORE H IN RWORK(LH)
350   H = HO
      RWORK(LH) = H
C
C     LOAD Y AND H*YPRIME INTO PHI(*,1) AND PHI(*,2)
      ITEMP = LPHI + NEQ
      DO 370 I = 1,NEQ
         RWORK(LPHI + I - 1) = Y(I)
370      RWORK(ITEMP + I - 1) = H*YPRIME(I)
C
390   GO TO 500
C
C-------------------------------------------------------
C     THIS BLOCK IS FOR CONTINUATION CALLS ONLY. ITS
C     PURPOSE IS TO CHECK STOP CONDITIONS BEFORE
C     TAKING A STEP.
C     ADJUST H IF NECESSARY TO MEET HMAX BOUND
C-------------------------------------------------------
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
C     CHECK WHETHER WE ARE WITHIN ROUNDOFF OF TSTOP
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
490   IF (DONE) GO TO 580
C
C-------------------------------------------------------
C     THE NEXT BLOCK CONTAINS THE CALL TO THE
C     ONE-STEP INTEGRATOR DDASTP.
C     THIS IS A LOOPING POINT FOR THE INTEGRATION STEPS.
C     CHECK FOR TOO MANY STEPS.
C     UPDATE WT.
C     CHECK FOR TOO MUCH ACCURACY REQUESTED.
C     COMPUTE MINIMUM STEPSIZE.
C-------------------------------------------------------
C
500   CONTINUE
C     CHECK FOR FAILURE TO COMPUTE INITIAL YPRIME
      IF (IDID .EQ. -12) GO TO 527
C
C     CHECK FOR TOO MANY STEPS
      IF((IWORK(LNST)-IWORK(LNSTL)).LT.IWORK(LMXSTP))
     *   GO TO 510
           IDID=-1
           GO TO 527
C
C     UPDATE WT
510   CALL DDAWTS(NEQ,INFO(2),RTOL,ATOL,RWORK(LPHI),
     *  RWORK(LWT),RPAR,IPAR)
      DO 520 I=1,NEQ
         IF(RWORK(I+LWT-1).GT.0.0D0)GO TO 520
           IDID=-3
           GO TO 527
520   CONTINUE
C
C     TEST FOR TOO MUCH ACCURACY REQUESTED.
      R=DDANRM(NEQ,RWORK(LPHI),RWORK(LWT),RPAR,IPAR)*
     *   100.0D0*UROUND
      IF(R.LE.1.0D0)GO TO 525
C     MULTIPLY RTOL AND ATOL BY R AND RETURN
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
C     COMPUTE MINIMUM STEPSIZE
      HMIN=4.0D0*UROUND*MAX(ABS(TN),ABS(TOUT))
C
C     TEST H VS. HMAX
      IF (INFO(7) .EQ. 0) GO TO 526
         RH = ABS(H)/RWORK(LHMAX)
         IF (RH .GT. 1.0D0) H = H/RH
526   CONTINUE
C
      CALL DDASTP(TN,Y,YPRIME,NEQ,
     *   RES,JAC,H,RWORK(LWT),INFO(1),IDID,RPAR,IPAR,
     *   RWORK(LPHI),RWORK(LDELTA),RWORK(LE),
     *   RWORK(LWM),IWORK(LIWM),
     *   RWORK(LALPHA),RWORK(LBETA),RWORK(LGAMMA),
     *   RWORK(LPSI),RWORK(LSIGMA),
     *   RWORK(LCJ),RWORK(LCJOLD),RWORK(LHOLD),
     *   RWORK(LS),HMIN,RWORK(LROUND),
     *   IWORK(LPHASE),IWORK(LJCALC),IWORK(LK),
     *   IWORK(LKOLD),IWORK(LNS),INFO(10),NTEMP)
527   IF(IDID.LT.0)GO TO 600
C
C--------------------------------------------------------
C     THIS BLOCK HANDLES THE CASE OF A SUCCESSFUL RETURN
C     FROM DDASTP (IDID=1).  TEST FOR STOP CONDITIONS.
C--------------------------------------------------------
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
      GO TO 580
C
C--------------------------------------------------------
C     ALL SUCCESSFUL RETURNS FROM DDASSL ARE MADE FROM
C     THIS BLOCK.
C--------------------------------------------------------
C
580   CONTINUE
      RWORK(LTN)=TN
      RWORK(LH)=H
      RETURN
C
C-----------------------------------------------------------------------
C     THIS BLOCK HANDLES ALL UNSUCCESSFUL
C     RETURNS OTHER THAN FOR ILLEGAL INPUT.
C-----------------------------------------------------------------------
C
600   CONTINUE
      ITEMP=-IDID
      GO TO (610,620,630,690,690,640,650,660,670,675,
     *  680,685), ITEMP
C
C     THE MAXIMUM NUMBER OF STEPS WAS TAKEN BEFORE
C     REACHING TOUT
610   WRITE (XERN3, '(1P,D15.6)') TN
      CALL XERMSG ('SLATEC', 'DDASSL',
     *   'AT CURRENT T = ' // XERN3 // ' 500 STEPS TAKEN ON THIS ' //
     *   'CALL BEFORE REACHING TOUT', IDID, 1)
      GO TO 690
C
C     TOO MUCH ACCURACY FOR MACHINE PRECISION
620   WRITE (XERN3, '(1P,D15.6)') TN
      CALL XERMSG ('SLATEC', 'DDASSL',
     *   'AT T = ' // XERN3 // ' TOO MUCH ACCURACY REQUESTED FOR ' //
     *   'PRECISION OF MACHINE. RTOL AND ATOL WERE INCREASED TO ' //
     *   'APPROPRIATE VALUES', IDID, 1)
      GO TO 690
C
C     WT(I) .LE. 0.0 FOR SOME I (NOT AT START OF PROBLEM)
630   WRITE (XERN3, '(1P,D15.6)') TN
      CALL XERMSG ('SLATEC', 'DDASSL',
     *   'AT T = ' // XERN3 // ' SOME ELEMENT OF WT HAS BECOME .LE. ' //
     *   '0.0', IDID, 1)
      GO TO 690
C
C     ERROR TEST FAILED REPEATEDLY OR WITH H=HMIN
640   WRITE (XERN3, '(1P,D15.6)') TN
      WRITE (XERN4, '(1P,D15.6)') H
      CALL XERMSG ('SLATEC', 'DDASSL',
     *   'AT T = ' // XERN3 // ' AND STEPSIZE H = ' // XERN4 //
     *   ' THE ERROR TEST FAILED REPEATEDLY OR WITH ABS(H)=HMIN',
     *   IDID, 1)
      GO TO 690
C
C     CORRECTOR CONVERGENCE FAILED REPEATEDLY OR WITH H=HMIN
650   WRITE (XERN3, '(1P,D15.6)') TN
      WRITE (XERN4, '(1P,D15.6)') H
      CALL XERMSG ('SLATEC', 'DDASSL',
     *   'AT T = ' // XERN3 // ' AND STEPSIZE H = ' // XERN4 //
     *   ' THE CORRECTOR FAILED TO CONVERGE REPEATEDLY OR WITH ' //
     *   'ABS(H)=HMIN', IDID, 1)
      GO TO 690
C
C     THE ITERATION MATRIX IS SINGULAR
660   WRITE (XERN3, '(1P,D15.6)') TN
      WRITE (XERN4, '(1P,D15.6)') H
      CALL XERMSG ('SLATEC', 'DDASSL',
     *   'AT T = ' // XERN3 // ' AND STEPSIZE H = ' // XERN4 //
     *   ' THE ITERATION MATRIX IS SINGULAR', IDID, 1)
      GO TO 690
C
C     CORRECTOR FAILURE PRECEEDED BY ERROR TEST FAILURES.
670   WRITE (XERN3, '(1P,D15.6)') TN
      WRITE (XERN4, '(1P,D15.6)') H
      CALL XERMSG ('SLATEC', 'DDASSL',
     *   'AT T = ' // XERN3 // ' AND STEPSIZE H = ' // XERN4 //
     *   ' THE CORRECTOR COULD NOT CONVERGE.  ALSO, THE ERROR TEST ' //
     *   'FAILED REPEATEDLY.', IDID, 1)
      GO TO 690
C
C     CORRECTOR FAILURE BECAUSE IRES = -1
675   WRITE (XERN3, '(1P,D15.6)') TN
      WRITE (XERN4, '(1P,D15.6)') H
      CALL XERMSG ('SLATEC', 'DDASSL',
     *   'AT T = ' // XERN3 // ' AND STEPSIZE H = ' // XERN4 //
     *   ' THE CORRECTOR COULD NOT CONVERGE BECAUSE IRES WAS EQUAL ' //
     *   'TO MINUS ONE', IDID, 1)
      GO TO 690
C
C     FAILURE BECAUSE IRES = -2
680   WRITE (XERN3, '(1P,D15.6)') TN
      WRITE (XERN4, '(1P,D15.6)') H
      CALL XERMSG ('SLATEC', 'DDASSL',
     *   'AT T = ' // XERN3 // ' AND STEPSIZE H = ' // XERN4 //
     *   ' IRES WAS EQUAL TO MINUS TWO', IDID, 1)
      GO TO 690
C
C     FAILED TO COMPUTE INITIAL YPRIME
685   WRITE (XERN3, '(1P,D15.6)') TN
      WRITE (XERN4, '(1P,D15.6)') HO
      CALL XERMSG ('SLATEC', 'DDASSL',
     *   'AT T = ' // XERN3 // ' AND STEPSIZE H = ' // XERN4 //
     *   ' THE INITIAL YPRIME COULD NOT BE COMPUTED', IDID, 1)
      GO TO 690
C
690   CONTINUE
      INFO(1)=-1
      T=TN
      RWORK(LTN)=TN
      RWORK(LH)=H
      RETURN
C
C-----------------------------------------------------------------------
C     THIS BLOCK HANDLES ALL ERROR RETURNS DUE
C     TO ILLEGAL INPUT, AS DETECTED BEFORE CALLING
C     DDASTP. FIRST THE ERROR MESSAGE ROUTINE IS
C     CALLED. IF THIS HAPPENS TWICE IN
C     SUCCESSION, EXECUTION IS TERMINATED
C
C-----------------------------------------------------------------------
701   CALL XERMSG ('SLATEC', 'DDASSL',
     *   'SOME ELEMENT OF INFO VECTOR IS NOT ZERO OR ONE', 1, 1)
      GO TO 750
C
702   WRITE (XERN1, '(I8)') NEQ
      CALL XERMSG ('SLATEC', 'DDASSL',
     *   'NEQ = ' // XERN1 // ' .LE. 0', 2, 1)
      GO TO 750
C
703   WRITE (XERN1, '(I8)') MXORD
      CALL XERMSG ('SLATEC', 'DDASSL',
     *   'MAXORD = ' // XERN1 // ' NOT IN RANGE', 3, 1)
      GO TO 750
C
704   WRITE (XERN1, '(I8)') LENRW
      WRITE (XERN2, '(I8)') LRW
      CALL XERMSG ('SLATEC', 'DDASSL',
     *   'RWORK LENGTH NEEDED, LENRW = ' // XERN1 //
     *   ', EXCEEDS LRW = ' // XERN2, 4, 1)
      GO TO 750
C
705   WRITE (XERN1, '(I8)') LENIW
      WRITE (XERN2, '(I8)') LIW
      CALL XERMSG ('SLATEC', 'DDASSL',
     *   'IWORK LENGTH NEEDED, LENIW = ' // XERN1 //
     *   ', EXCEEDS LIW = ' // XERN2, 5, 1)
      GO TO 750
C
706   CALL XERMSG ('SLATEC', 'DDASSL',
     *   'SOME ELEMENT OF RTOL IS .LT. 0', 6, 1)
      GO TO 750
C
707   CALL XERMSG ('SLATEC', 'DDASSL',
     *   'SOME ELEMENT OF ATOL IS .LT. 0', 7, 1)
      GO TO 750
C
708   CALL XERMSG ('SLATEC', 'DDASSL',
     *   'ALL ELEMENTS OF RTOL AND ATOL ARE ZERO', 8, 1)
      GO TO 750
C
709   WRITE (XERN3, '(1P,D15.6)') TSTOP
      WRITE (XERN4, '(1P,D15.6)') TOUT
      CALL XERMSG ('SLATEC', 'DDASSL',
     *   'INFO(4) = 1 AND TSTOP = ' // XERN3 // ' BEHIND TOUT = ' //
     *   XERN4, 9, 1)
      GO TO 750
C
710   WRITE (XERN3, '(1P,D15.6)') HMAX
      CALL XERMSG ('SLATEC', 'DDASSL',
     *   'HMAX = ' // XERN3 // ' .LT. 0.0', 10, 1)
      GO TO 750
C
711   WRITE (XERN3, '(1P,D15.6)') TOUT
      WRITE (XERN4, '(1P,D15.6)') T
      CALL XERMSG ('SLATEC', 'DDASSL',
     *   'TOUT = ' // XERN3 // ' BEHIND T = ' // XERN4, 11, 1)
      GO TO 750
C
712   CALL XERMSG ('SLATEC', 'DDASSL',
     *   'INFO(8)=1 AND H0=0.0', 12, 1)
      GO TO 750
C
713   CALL XERMSG ('SLATEC', 'DDASSL',
     *   'SOME ELEMENT OF WT IS .LE. 0.0', 13, 1)
      GO TO 750
C
714   WRITE (XERN3, '(1P,D15.6)') TOUT
      WRITE (XERN4, '(1P,D15.6)') T
      CALL XERMSG ('SLATEC', 'DDASSL',
     *   'TOUT = ' // XERN3 // ' TOO CLOSE TO T = ' // XERN4 //
     *   ' TO START INTEGRATION', 14, 1)
      GO TO 750
C
715   WRITE (XERN3, '(1P,D15.6)') TSTOP
      WRITE (XERN4, '(1P,D15.6)') T
      CALL XERMSG ('SLATEC', 'DDASSL',
     *   'INFO(4)=1 AND TSTOP = ' // XERN3 // ' BEHIND T = ' // XERN4,
     *   15, 1)
      GO TO 750
C
716   WRITE (XERN1, '(I8)') MXSTP
      CALL XERMSG ('SLATEC', 'DDASSL',
     *   'INFO(12)=1 AND MXSTP = ' // XERN1 // ' ILLEGAL.', 3, 1)
      GO TO 750
C
717   WRITE (XERN1, '(I8)') IWORK(LML)
      CALL XERMSG ('SLATEC', 'DDASSL',
     *   'ML = ' // XERN1 // ' ILLEGAL.  EITHER .LT. 0 OR .GT. NEQ',
     *   17, 1)
      GO TO 750
C
718   WRITE (XERN1, '(I8)') IWORK(LMU)
      CALL XERMSG ('SLATEC', 'DDASSL',
     *   'MU = ' // XERN1 // ' ILLEGAL.  EITHER .LT. 0 OR .GT. NEQ',
     *   18, 1)
      GO TO 750
C
719   WRITE (XERN3, '(1P,D15.6)') TOUT
      CALL XERMSG ('SLATEC', 'DDASSL',
     *  'TOUT = T = ' // XERN3, 19, 1)
      GO TO 750
C
750   IDID=-33
      IF(INFO(1).EQ.-1) THEN
         CALL XERMSG ('SLATEC', 'DDASSL',
     *      'REPEATED OCCURRENCES OF ILLEGAL INPUT$$' //
     *      'RUN TERMINATED. APPARENT INFINITE LOOP', -999, 2)
      ENDIF
C
      INFO(1)=-1
      RETURN
C-----------END OF SUBROUTINE DDASSL------------------------------------
      END

C Work performed under the auspices of the U.S. Department of Energy
C by Lawrence Livermore National Laboratory under contract number
C W-7405-Eng-48.
C
      SUBROUTINE DSLVD(NEQ,DELTA,WM,IWM)
C
C***BEGIN PROLOGUE  DSLVD
C***REFER TO  DDASPK
C***DATE WRITTEN   890101   (YYMMDD)
C***REVISION DATE  900926   (YYMMDD)
C***REVISION DATE  940701   (YYMMDD) (new LIPVT)
C
C-----------------------------------------------------------------------
C***DESCRIPTION
C
C     This routine manages the solution of the linear
C     system arising in the Newton iteration.
C     Real matrix information and real temporary storage
C     is stored in the array WM.
C     Integer matrix information is stored in the array IWM.
C     For a dense matrix, the LAPACK routine DGETRS is called.
C     For a banded matrix, the LAPACK routine DGBTRS is called.
C-----------------------------------------------------------------------
C***ROUTINES CALLED
C   DGETRS, DGBTRS
C
C***END PROLOGUE  DSLVD
C
C
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      DIMENSION DELTA(*),WM(*),IWM(*)
C
      PARAMETER (LML=1, LMU=2, LMTYPE=4, LLCIWP=30)
C
      LIPVT = IWM(LLCIWP)
      MTYPE=IWM(LMTYPE)
      GO TO(100,100,300,400,400),MTYPE
C
C     Dense matrix.
C
100   CALL DGETRS('N', NEQ, 1, WM, NEQ, IWM(LIPVT), DELTA, NEQ, INLPCK)
      RETURN
C
C     Dummy section for MTYPE=3.
C
300   CONTINUE
      RETURN
C
C     Banded matrix.
C
400   MEBAND=2*IWM(LML)+IWM(LMU)+1
      CALL DGBTRS('N', NEQ, IWM(LML), IWM(LMU), 1, WM, MEBAND,
     *     IWM(LIPVT), DELTA, NEQ, INLPCK)
      RETURN
C
C------END OF SUBROUTINE DSLVD------------------------------------------
      END

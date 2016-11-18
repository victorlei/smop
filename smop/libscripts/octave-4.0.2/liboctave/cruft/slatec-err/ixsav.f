*DECK IXSAV
      INTEGER FUNCTION IXSAV (IPAR, IVALUE, ISET)
C***BEGIN PROLOGUE  IXSAV
C***SUBSIDIARY
C***PURPOSE  Save and recall error message control parameters.
C***LIBRARY   MATHLIB
C***CATEGORY  R3C
C***TYPE      ALL (IXSAV-A)
C***AUTHOR  Hindmarsh, Alan C., (LLNL)
C***DESCRIPTION
C
C  IXSAV saves and recalls one of two error message parameters:
C    LUNIT, the logical unit number to which messages are printed, and
C    MESFLG, the message print flag.
C  This is a modification of the SLATEC library routine J4SAVE.
C
C  Saved local variables..
C   LUNIT  = Logical unit number for messages.
C   LUNDEF = Default logical unit number, data-loaded to 6 below
C            (may be machine-dependent).
C   MESFLG = Print control flag..
C            1 means print all messages (the default).
C            0 means no printing.
C
C  On input..
C    IPAR   = Parameter indicator (1 for LUNIT, 2 for MESFLG).
C    IVALUE = The value to be set for the parameter, if ISET = .TRUE.
C    ISET   = Logical flag to indicate whether to read or write.
C             If ISET = .TRUE., the parameter will be given
C             the value IVALUE.  If ISET = .FALSE., the parameter
C             will be unchanged, and IVALUE is a dummy argument.
C
C  On return..
C    IXSAV = The (old) value of the parameter.
C
C***SEE ALSO  XERMSG, XERRWD, XERRWV
C***ROUTINES CALLED  NONE
C***REVISION HISTORY  (YYMMDD)
C   921118  DATE WRITTEN
C   930329  Modified prologue to SLATEC format. (FNF)
C   941025  Minor modification re default unit number. (ACH)
C***END PROLOGUE  IXSAV
C
C**End
      LOGICAL ISET
      INTEGER IPAR, IVALUE
C-----------------------------------------------------------------------
      INTEGER LUNIT, LUNDEF, MESFLG
C-----------------------------------------------------------------------
C The following Fortran-77 declaration is to cause the values of the
C listed (local) variables to be saved between calls to this routine.
C-----------------------------------------------------------------------
      SAVE LUNIT, LUNDEF, MESFLG
      DATA LUNIT/-1/, LUNDEF/6/, MESFLG/1/
C
C***FIRST EXECUTABLE STATEMENT  IXSAV
      IF (IPAR .EQ. 1) THEN
        IF (LUNIT .EQ. -1) LUNIT = LUNDEF
        IXSAV = LUNIT
        IF (ISET) LUNIT = IVALUE
        ENDIF
C
      IF (IPAR .EQ. 2) THEN
        IXSAV = MESFLG
        IF (ISET) MESFLG = IVALUE
        ENDIF
C
      RETURN
C----------------------- End of Function IXSAV -------------------------
      END

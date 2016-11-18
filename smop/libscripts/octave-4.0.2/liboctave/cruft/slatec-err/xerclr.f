*DECK XERCLR
      SUBROUTINE XERCLR
C***BEGIN PROLOGUE  XERCLR
C***PURPOSE  Reset current error number to zero.
C***LIBRARY   SLATEC (XERROR)
C***CATEGORY  R3C
C***TYPE      ALL (XERCLR-A)
C***KEYWORDS  ERROR, XERROR
C***AUTHOR  Jones, R. E., (SNLA)
C***DESCRIPTION
C
C     Abstract
C        This routine simply resets the current error number to zero.
C        This may be necessary in order to determine that a certain
C        error has occurred again since the last time NUMXER was
C        referenced.
C
C***REFERENCES  R. E. Jones and D. K. Kahaner, XERROR, the SLATEC
C                 Error-handling Package, SAND82-0800, Sandia
C                 Laboratories, 1982.
C***ROUTINES CALLED  J4SAVE
C***REVISION HISTORY  (YYMMDD)
C   790801  DATE WRITTEN
C   861211  REVISION DATE from Version 3.2
C   891214  Prologue converted to Version 4.0 format.  (BAB)
C   920501  Reformatted the REFERENCES section.  (WRB)
C***END PROLOGUE  XERCLR
C***FIRST EXECUTABLE STATEMENT  XERCLR
      JUNK = J4SAVE(1,0,.TRUE.)
      RETURN
      END

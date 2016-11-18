*DECK XGETF
      SUBROUTINE XGETF (KONTRL)
C***BEGIN PROLOGUE  XGETF
C***PURPOSE  Return the current value of the error control flag.
C***LIBRARY   SLATEC (XERROR)
C***CATEGORY  R3C
C***TYPE      ALL (XGETF-A)
C***KEYWORDS  ERROR, XERROR
C***AUTHOR  Jones, R. E., (SNLA)
C***DESCRIPTION
C
C   Abstract
C        XGETF returns the current value of the error control flag
C        in KONTRL.  See subroutine XSETF for flag value meanings.
C        (KONTRL is an output parameter only.)
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
C***END PROLOGUE  XGETF
C***FIRST EXECUTABLE STATEMENT  XGETF
      KONTRL = J4SAVE(2,0,.FALSE.)
      RETURN
      END

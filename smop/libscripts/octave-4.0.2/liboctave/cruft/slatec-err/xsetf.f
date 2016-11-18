*DECK XSETF
      SUBROUTINE XSETF (KONTRL)
C***BEGIN PROLOGUE  XSETF
C***PURPOSE  Set the error control flag.
C***LIBRARY   SLATEC (XERROR)
C***CATEGORY  R3A
C***TYPE      ALL (XSETF-A)
C***KEYWORDS  ERROR, XERROR
C***AUTHOR  Jones, R. E., (SNLA)
C***DESCRIPTION
C
C     Abstract
C        XSETF sets the error control flag value to KONTRL.
C        (KONTRL is an input parameter only.)
C        The following table shows how each message is treated,
C        depending on the values of KONTRL and LEVEL.  (See XERMSG
C        for description of LEVEL.)
C
C        If KONTRL is zero or negative, no information other than the
C        message itself (including numeric values, if any) will be
C        printed.  If KONTRL is positive, introductory messages,
C        trace-backs, etc., will be printed in addition to the message.
C
C              ABS(KONTRL)
C        LEVEL        0              1              2
C        value
C          2        fatal          fatal          fatal
C
C          1     not printed      printed         fatal
C
C          0     not printed      printed        printed
C
C         -1     not printed      printed        printed
C                                  only           only
C                                  once           once
C
C***REFERENCES  R. E. Jones and D. K. Kahaner, XERROR, the SLATEC
C                 Error-handling Package, SAND82-0800, Sandia
C                 Laboratories, 1982.
C***ROUTINES CALLED  J4SAVE, XERMSG
C***REVISION HISTORY  (YYMMDD)
C   790801  DATE WRITTEN
C   890531  Changed all specific intrinsics to generic.  (WRB)
C   890531  REVISION DATE from Version 3.2
C   891214  Prologue converted to Version 4.0 format.  (BAB)
C   900510  Change call to XERRWV to XERMSG.  (RWC)
C   920501  Reformatted the REFERENCES section.  (WRB)
C***END PROLOGUE  XSETF
      CHARACTER *8 XERN1
C***FIRST EXECUTABLE STATEMENT  XSETF
      IF (ABS(KONTRL) .GT. 2) THEN
         WRITE (XERN1, '(I8)') KONTRL
         CALL XERMSG ('SLATEC', 'XSETF',
     *      'INVALID ARGUMENT = ' // XERN1, 1, 2)
         RETURN
      ENDIF
C
      JUNK = J4SAVE(2,KONTRL,.TRUE.)
      RETURN
      END

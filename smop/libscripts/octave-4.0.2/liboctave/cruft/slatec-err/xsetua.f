*DECK XSETUA
      SUBROUTINE XSETUA (IUNITA, N)
C***BEGIN PROLOGUE  XSETUA
C***PURPOSE  Set logical unit numbers (up to 5) to which error
C            messages are to be sent.
C***LIBRARY   SLATEC (XERROR)
C***CATEGORY  R3B
C***TYPE      ALL (XSETUA-A)
C***KEYWORDS  ERROR, XERROR
C***AUTHOR  Jones, R. E., (SNLA)
C***DESCRIPTION
C
C     Abstract
C        XSETUA may be called to declare a list of up to five
C        logical units, each of which is to receive a copy of
C        each error message processed by this package.
C        The purpose of XSETUA is to allow simultaneous printing
C        of each error message on, say, a main output file,
C        an interactive terminal, and other files such as graphics
C        communication files.
C
C     Description of Parameters
C      --Input--
C        IUNIT - an array of up to five unit numbers.
C                Normally these numbers should all be different
C                (but duplicates are not prohibited.)
C        N     - the number of unit numbers provided in IUNIT
C                must have 1 .LE. N .LE. 5.
C
C***REFERENCES  R. E. Jones and D. K. Kahaner, XERROR, the SLATEC
C                 Error-handling Package, SAND82-0800, Sandia
C                 Laboratories, 1982.
C***ROUTINES CALLED  J4SAVE, XERMSG
C***REVISION HISTORY  (YYMMDD)
C   790801  DATE WRITTEN
C   861211  REVISION DATE from Version 3.2
C   891214  Prologue converted to Version 4.0 format.  (BAB)
C   900510  Change call to XERRWV to XERMSG.  (RWC)
C   920501  Reformatted the REFERENCES section.  (WRB)
C***END PROLOGUE  XSETUA
      DIMENSION IUNITA(5)
      CHARACTER *8 XERN1
C***FIRST EXECUTABLE STATEMENT  XSETUA
C
      IF (N.LT.1 .OR. N.GT.5) THEN
         WRITE (XERN1, '(I8)') N
         CALL XERMSG ('SLATEC', 'XSETUA',
     *      'INVALID NUMBER OF UNITS, N = ' // XERN1, 1, 2)
         RETURN
      ENDIF
C
      DO 10 I=1,N
         INDEX = I+4
         IF (I.EQ.1) INDEX = 3
         JUNK = J4SAVE(INDEX,IUNITA(I),.TRUE.)
   10 CONTINUE
      JUNK = J4SAVE(5,N,.TRUE.)
      RETURN
      END

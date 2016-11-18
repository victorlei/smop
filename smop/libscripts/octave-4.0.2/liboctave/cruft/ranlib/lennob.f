      INTEGER FUNCTION lennob(string)
      IMPLICIT INTEGER (a-p,r-z),LOGICAL (q)
C**********************************************************************
C
C     INTEGER FUNCTION LENNOB( STRING )
C                LENgth NOt counting trailing Blanks
C
C
C                              Function
C
C
C     Returns the length of STRING up to and including the last
C     non-blank character.
C
C
C                              Arguments
C
C
C     STRING --> String whose length not counting trailing blanks
C                is returned.
C
C**********************************************************************
      CHARACTER*(*) string

      end = len(string)
      DO 20,i = end,1,-1
          IF (.NOT. (string(i:i).NE.' ')) GO TO 10
          lennob = i
          RETURN

   10     CONTINUE
   20 CONTINUE
      lennob = 0
      RETURN

      END
